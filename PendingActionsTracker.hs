{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module PendingActionsTracker
    ( PendingActionsState
    , BridgewalkerAccount(..)
    , BridgewalkerAction(..)
    , initialPendingActionsState
    , initPendingActionsTracker
    , nudgePendingActionsTracker
    , addPendingActions
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Error
import Control.Monad
import Database.PostgreSQL.Simple
import Data.List
import Data.Serialize
import Data.Time.Clock
import GHC.Generics
import Network.MtGoxAPI

import qualified Data.Text as T
import qualified Network.BitcoinRPC as RPC
import qualified Data.Sequence as S

import AddressUtils
import Config
import ConfigTypes
import LoggingUtils

pauseInterval :: NominalDiffTime
pauseInterval = 60  -- pauses are 60 seconds long

data PendingActionsState = PendingActionsState
                                { pasSequence :: S.Seq BridgewalkerAction
                                , pasStatus :: String
                                }
                           deriving (Show, Generic)

data PendingActionsStateModification = RemoveAction
                                     | ReplaceAction BridgewalkerAction
                                        -- this is the replacement action
                                     | KeepAction
                                     | AddPauseAction String
                                        -- parameter describes reason for pause

data BridgewalkerAccount = BridgewalkerAccount { bAccount :: Integer }
                           deriving (Generic, Show)

data WithdrawalType = WithdrawBTC { wtAmount :: Integer }
                    | WithdrawUSD { wtAmount :: Integer }
                    deriving (Show)

data WithdrawalAction = WithdrawalAction { waAddress :: RPC.BitcoinAddress
                                         , waType :: WithdrawalType
                                         }
                        deriving (Show)

data BridgewalkerAction = DepositAction { baAmount :: Integer
                                        , baAddress :: RPC.BitcoinAddress
                                        }
                        | SellBTCAction { baAmount :: Integer
                                        , baAccount :: BridgewalkerAccount
                                        }
                        | PauseAction { baExpiration :: UTCTime }
                        -- TODO: more actions
                        deriving (Show, Generic)

data SellOrderProblem = MtGoxLowBalance | MtGoxCallError String

instance Serialize BridgewalkerAccount

instance Serialize BridgewalkerAction

instance Serialize PendingActionsState

initialPendingActionsState :: PendingActionsState
initialPendingActionsState = PendingActionsState { pasSequence = S.empty
                                                 , pasStatus = ""
                                                 }

initPendingActionsTracker :: (Connection -> IO (PendingActionsState)) -> (Connection -> PendingActionsState -> IO ()) -> BridgewalkerHandles -> IO (PendingActionsTrackerHandle)
initPendingActionsTracker readState writeState bwHandles = do
    chan <- newChan
    forkIO $ trackerLoop readState writeState bwHandles chan
    let handle = PendingActionsTrackerHandle chan
    nudgePendingActionsTracker handle
    return handle

nudgePendingActionsTracker :: PendingActionsTrackerHandle -> IO ()
nudgePendingActionsTracker (PendingActionsTrackerHandle chan) =
    writeChan chan ()

trackerLoop :: (Connection -> IO (PendingActionsState)) -> (Connection -> PendingActionsState -> IO ()) -> BridgewalkerHandles -> Chan () -> IO ()
trackerLoop readState writeState bwHandles chan =
    let dbConn = bhDBConn bwHandles
    in forever $ do
        _ <- readChan chan
        withTransaction dbConn $ do
            paState <- readState dbConn
            (paState', keepGoing) <- maybeProcessOneAction bwHandles paState
            writeState dbConn paState'
            when keepGoing $ writeChan chan ()

maybeProcessOneAction :: BridgewalkerHandles-> PendingActionsState -> IO (PendingActionsState, Bool)
maybeProcessOneAction bwHandles paState =
     case popPendingAction paState of
            Nothing -> return (paState, False)
            Just (action, paState') -> processOneAction bwHandles action paState'

processOneAction :: BridgewalkerHandles-> BridgewalkerAction-> PendingActionsState-> IO (PendingActionsState, Bool)
processOneAction bwHandles action paState' = do
    modification <- case action of
        DepositAction amount address ->
            processDeposit bwHandles amount address
        SellBTCAction amount account ->
            sellBTC bwHandles amount account
        PauseAction expiration ->
            checkPause expiration
    case modification of
        RemoveAction ->
            -- nothing to be done, action has already been popped off;
            -- clear status and keep processing
            return (paState' { pasStatus = "" }, True)
        ReplaceAction newAction ->
            -- add another action in place of the one that was just
            -- removed and keep processing
            let paState'' = putPendingAction paState' newAction
            in return (paState'' { pasStatus = "" }, True)
        KeepAction ->
            -- put action back in the queue
            return (putPendingAction paState' action, False)
        AddPauseAction status -> do
            -- put action back in the queue and also add
            -- a pause action
            let paState'' = putPendingAction paState' action
            expiration <- addUTCTime pauseInterval <$> getCurrentTime
            let paState''' = putPendingAction paState'' $ PauseAction expiration
            return (paState''' { pasStatus = status }, False)

checkPause :: UTCTime -> IO PendingActionsStateModification
checkPause expiration = do
    now <- getCurrentTime
    return $ if now >= expiration
                then RemoveAction
                else KeepAction

processDeposit :: BridgewalkerHandles-> Integer -> RPC.BitcoinAddress -> IO PendingActionsStateModification
processDeposit bwHandles amount address =
    let dbConn = bhDBConn bwHandles
        logger = bhAppLogger bwHandles
        minimalOrderBTC = bcMtGoxMinimalOrderBTC . bhConfig $ bwHandles
        magicAddress = RPC.BitcoinAddress "13NsGekvJKyGTeteu6fjyTxKbpkBox58j8"
        magicAccount = 1 :: Integer
    in if adjustAddr address == magicAddress
        then do
            btcBalance <- getBTCBalance dbConn magicAccount
            let newBalance = btcBalance + amount
            execute dbConn
                        "update accounts set btc_balance=? where account_nr=?"
                        (newBalance, magicAccount)
            let logMsg = DepositProcessed
                            { lcAccount = magicAccount
                            , lcInfo = show amount
                                        ++ " BTC deposited into account "
                                        ++ show magicAccount
                                        ++ " - balance is now "
                                        ++ show newBalance ++ " BTC."
                            }
            logger logMsg
            return $ if newBalance >= minimalOrderBTC
                        then let action = SellBTCAction
                                            { baAmount = newBalance
                                            , baAccount =
                                                BridgewalkerAccount magicAccount
                                            }
                             in ReplaceAction action
                        else RemoveAction
        else do
            let logMsg = LogMisc { lcInfo = "Ignoring deposit to\
                                             \ non-magical address." }
            logger logMsg
            return RemoveAction

sellBTC :: BridgewalkerHandles-> Integer-> BridgewalkerAccount-> IO PendingActionsStateModification
sellBTC bwHandles amount bwAccount = do
    let mtgoxHandles = bhMtGoxHandles bwHandles
        safetyMarginBTC = bcSafetyMarginBTC . bhConfig $ bwHandles
        logger = bhAppLogger bwHandles
        dbConn = bhDBConn bwHandles
        account = bAccount $ bwAccount
    sell <- tryToExecuteSellOrder mtgoxHandles safetyMarginBTC amount
    case sell of
        Left (MtGoxCallError msg) -> do
            let logMsg = MtGoxError
                            { lcInfo = "Error communicating with Mt.Gox while\
                                       \ attempting to sell BTC. Error was: \
                                       \ " ++ msg }
            logger logMsg
            return $ AddPauseAction "Communication problems with Mt.Gox\
                                    \ - pausing until it is resolved."
        Left MtGoxLowBalance -> do
            let logMsg = MtGoxLowBTCBalance
                        { lcInfo = "Postponing BTC sell order because of low\
                                   \ balance in Mt.Gox account." }
            logger logMsg
            return $ AddPauseAction "Pausing until rebalancing of\
                                    \ reserves is completed."
        Right stats -> do
            let usdAmount = max 0 (usdEarned stats - usdFee stats)
            btcBalance <- getBTCBalance dbConn account
            usdBalance <- getUSDBalance dbConn account
            let newBTCBalance = max 0 (btcBalance - amount)
                newUSDBalance = usdBalance + usdAmount
            execute dbConn "update accounts set btc_balance=?, usd_balance=?\
                                \ where account_nr=?"
                                (newBTCBalance, newUSDBalance, account)
            let logMsg = BTCSold
                            { lcAccount = account
                            , lcInfo = show amount
                                        ++ " BTC sold on Mt.Gox and credited "
                                        ++ show usdAmount ++ " USD to account "
                                        ++ show account ++ " - balance is now "
                                        ++ show newUSDBalance ++ " USD and "
                                        ++ show newBTCBalance ++ " BTC."
                            }
            logger logMsg
            return RemoveAction

tryToExecuteSellOrder :: MtGoxAPIHandles-> Integer -> Integer -> IO (Either SellOrderProblem OrderStats)
tryToExecuteSellOrder mtgoxHandles safetyMarginBTC amount = runEitherT $ do
    privateInfo <- noteT (MtGoxCallError "Unable to call getPrivateInfoR.")
                    . MaybeT $ callHTTPApi mtgoxHandles getPrivateInfoR
    _ <- tryAssert MtGoxLowBalance
            (piBtcBalance privateInfo >= amount + safetyMarginBTC) ()
    orderStats <- EitherT $
        adjustError <$> callHTTPApi mtgoxHandles submitOrder
                            OrderTypeSellBTC (adjustAmount amount)
    return orderStats
  where
    adjustError (Left err) = Left (MtGoxCallError err)
    adjustError (Right result) = Right result

getBTCBalance :: Connection -> Integer -> IO Integer
getBTCBalance dbConn account = do
    let errMsg = "Expected to find account " ++ show account
                    ++ " while doing getBTCBalance, but failed."
    Only balance <- expectOneRow errMsg <$>
        query dbConn "select btc_balance from accounts where account_nr=?"
                        (Only account)
    return balance

getUSDBalance :: Connection -> Integer -> IO Integer
getUSDBalance dbConn account = do
    let errMsg = "Expected to find account " ++ show account
                    ++ " while doing getUSDBalance, but failed."
    Only balance <- expectOneRow errMsg <$>
        query dbConn "select usd_balance from accounts where account_nr=?"
                        (Only account)
    return balance

expectOneRow :: String -> [a] -> a
expectOneRow errMsg [] = error errMsg
expectOneRow _ (x:_) = x

popPendingAction :: PendingActionsState-> Maybe (BridgewalkerAction, PendingActionsState)
popPendingAction state =
    let sequence = pasSequence state
    in if S.null sequence
            then Nothing
            else let (a, as) = S.splitAt 1 sequence
                     action = S.index a 0   -- should always succeed as
                                            -- we checked that the sequence
                                            -- is not empty
                 in Just (action, state { pasSequence = as })

-- | Add a new action to the front of the pending actions queue.
putPendingAction state action =
    let sequence = pasSequence state
    in state { pasSequence = action S.<| sequence }

-- | Add a new action to the end of the pending actions queue.
addPendingAction :: PendingActionsState-> BridgewalkerAction -> PendingActionsState
addPendingAction state action =
    let sequence = pasSequence state
    in state { pasSequence = sequence S.|> action }

addPendingActions :: PendingActionsState-> [BridgewalkerAction] -> PendingActionsState
addPendingActions = foldl' addPendingAction
