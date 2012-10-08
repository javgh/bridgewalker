{-# LANGUAGE OverloadedStrings #-}
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
import Data.Time.Clock
import Network.MtGoxAPI

import qualified Data.Text as T
import qualified Network.BitcoinRPC as RPC
import qualified Data.Sequence as S

import AddressUtils
import CommonTypes
import Config
import DbUtils
import LoggingUtils

pauseInterval :: NominalDiffTime
pauseInterval = 60  -- pauses are 60 seconds long

data PendingActionsStateModification = RemoveAction
                                     | ReplaceAction BridgewalkerAction
                                        -- this is the replacement action
                                     | KeepAction
                                     | AddPauseAction String
                                        -- parameter describes reason for pause

data WithdrawalType = WithdrawBTC { wtAmount :: Integer }
                    | WithdrawUSD { wtAmount :: Integer }
                    deriving (Show)

data WithdrawalAction = WithdrawalAction { waAddress :: RPC.BitcoinAddress
                                         , waType :: WithdrawalType
                                         }
                        deriving (Show)

data SellOrderProblem = MtGoxLowBalance | MtGoxCallError String

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
        BuyBTCAction amount address account ->
            buyBTC bwHandles amount address account
        SendBTCAction amount address account ->
            sendBTC bwHandles amount address account
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
        magicAddress = RPC.BitcoinAddress "17cWnmBb4b8EMrHhSiasMXsbsc1ru7iTGj"
        magicAccount = 1 :: Integer
    in if adjustAddr address == magicAddress
        then do
            btcBalance <- getBTCInBalance dbConn magicAccount
            let newBalance = btcBalance + amount
            execute dbConn
                        "update accounts set btc_in=? where account_nr=?"
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
            btcBalance <- getBTCInBalance dbConn account
            usdBalance <- getUSDBalance dbConn account
            let newBTCBalance = max 0 (btcBalance - amount)
                newUSDBalance = usdBalance + usdAmount
            execute dbConn "update accounts set btc_in=?, usd_balance=?\
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

buyBTC :: BridgewalkerHandles-> Integer-> RPC.BitcoinAddress-> BridgewalkerAccount-> IO PendingActionsStateModification
buyBTC bwHandles amount address bwAccount = do
    let mtgoxHandles = bhMtGoxHandles bwHandles
        depthStoreHandle = mtgoxDepthStoreHandle mtgoxHandles
        safetyMarginUSD = bcSafetyMarginUSD . bhConfig $ bwHandles
        logger = bhAppLogger bwHandles
        dbConn = bhDBConn bwHandles
        account = bAccount $ bwAccount
    -- TODO: check for minimal BTC amount
    usdAmountM <- simulateBTCBuy depthStoreHandle amount
    case usdAmountM of
        Nothing -> do
            let logMsg = MtGoxError
                            { lcInfo = "Stale data from depth store while\
                                       \ trying to calculate USD value of\
                                       \ BTC buy order." }
            logger logMsg
            return $ AddPauseAction "Communication problems with Mt.Gox\
                                    \ - pausing until it is resolved."
        Just usdAmount -> do
            usdBalance <- getUSDBalance dbConn account
            -- TODO: take fees into account
            if usdAmount > usdBalance
                then return RemoveAction    -- TODO: figure out way in which to inform user;
                                            --       probably via transaction log
                else do
                    buy <- tryToExecuteBuyOrder mtgoxHandles safetyMarginUSD
                                amount usdAmount
                    processBuyResult logger dbConn bwAccount amount address buy

processBuyResult :: Logger-> Connection-> BridgewalkerAccount-> Integer-> RPC.BitcoinAddress-> Either SellOrderProblem OrderStats-> IO PendingActionsStateModification
processBuyResult logger dbConn bwAccount btcAmount address buy = do
    let account = bAccount $ bwAccount
    case buy of
        Left (MtGoxCallError msg) -> do
            let logMsg = MtGoxError
                            { lcInfo = "Error communicating with Mt.Gox while\
                                       \ attempting to buy BTC. Error was: \
                                       \ " ++ msg }
            logger logMsg
            return $ AddPauseAction "Communication problems with Mt.Gox\
                                    \ - pausing until it is resolved."
        Left MtGoxLowBalance -> do
            let logMsg = MtGoxLowBTCBalance
                        { lcInfo = "Postponing BTC buy order because of low\
                                   \ balance in Mt.Gox account." }
            logger logMsg
            return $ AddPauseAction "Hot wallet is running low - pausing\
                                    \ until resolved via cold reserves."
        Right stats -> do
            let usdAmount = usdSpent stats + usdFee stats
            usdBalance <- getUSDBalance dbConn account
            -- TODO: deal with case when usdAmount does end up lower
            --       than usdBalance, because things changed last minute
            let newUSDBalance = max 0 (usdBalance - usdAmount)
            execute dbConn "update accounts set usd_balance=?\
                                \ where account_nr=?"
                                (newUSDBalance, account)
            let logMsg = BTCBought
                            { lcAccount = account
                            , lcInfo = show btcAmount
                                        ++ " BTC bought on Mt.Gox and subtracted "
                                        ++ show usdAmount ++ " USD from account "
                                        ++ show account ++ " - balance is now "
                                        ++ show newUSDBalance ++ " USD."
                            }
            logger logMsg
            let action = SendBTCAction { baAmount = btcAmount
                                       , CommonTypes.baAddress
                                            = address
                                       , baAccount = bwAccount
                                       }
            return $ ReplaceAction action

sendBTC :: BridgewalkerHandles-> Integer-> RPC.BitcoinAddress-> BridgewalkerAccount-> IO PendingActionsStateModification
sendBTC bwHandles amount address bwAccount = do
    let rpcAuth = bcRPCAuth . bhConfig $ bwHandles
        safetyMarginBTC = bcSafetyMarginBTC . bhConfig $ bwHandles
        logger = bhAppLogger bwHandles
        account = bAccount $ bwAccount
        mLogger = Nothing   -- TODO: find better solution for watchdog logger
    btcSystemBalance <- RPC.getBalanceR mLogger rpcAuth
                                            confsNeededForSending True
    if (amount + safetyMarginBTC <= adjustAmount btcSystemBalance)
        then do
            result <- RPC.sendToAddress rpcAuth address (adjustAmount amount)
            case result of
                Left networkOrParseError -> do
                    let logMsg = BTCSendNetworkOrParseError
                                    { lcAccount = account
                                    , lcAddress =
                                        T.unpack . RPC.btcAddress $ address
                                    , lcAmount = amount
                                    , lcInfo = networkOrParseError
                                    }
                    logger logMsg
                    return RemoveAction
                Right result' -> do
                    case result' of
                        Left sendError -> do
                            let logMsg = BTCSendError
                                            { lcAccount = account
                                            , lcAddress =
                                                T.unpack . RPC.btcAddress
                                                                    $ address
                                            , lcAmount = amount
                                            , lcInfo = show sendError
                                            }
                            logger logMsg
                            return RemoveAction
                        Right txID -> do
                            let logMsg = BTCSent
                                            { lcAccount = account
                                            , lcInfo = "Account "
                                                ++ show account ++ " sent out "
                                                ++ show amount ++ " BTC to "
                                                ++ (T.unpack . RPC.btcAddress
                                                                    $ address)
                                                ++ " with transaction "
                                                ++ (T.unpack . RPC.btcTxID
                                                                    $ txID)
                                                ++ "."
                                            }
                            logger logMsg
                            return RemoveAction
        else do
            let logMsg = BitcoindLowBTCBalance
                        { lcInfo = "Postponing BTC send action because of low\
                                   \ local BTC balance." }
            logger logMsg
            return $ AddPauseAction "Pausing until rebalancing of\
                                    \ reserves is completed."

tryToExecuteSellOrder :: MtGoxAPIHandles-> Integer -> Integer -> IO (Either SellOrderProblem OrderStats)
tryToExecuteSellOrder mtgoxHandles safetyMarginBTC amount = runEitherT $ do
    privateInfo <- noteT (MtGoxCallError "Unable to call getPrivateInfoR.")
                    . MaybeT $ callHTTPApi mtgoxHandles getPrivateInfoR
    _ <- tryAssert MtGoxLowBalance
            (piBtcBalance privateInfo >= amount + safetyMarginBTC) ()
    orderStats <- EitherT $
        adjustMtGoxError <$> callHTTPApi mtgoxHandles submitOrder
                                OrderTypeSellBTC amount
    return orderStats

tryToExecuteBuyOrder :: MtGoxAPIHandles-> Integer-> Integer-> Integer-> IO (Either SellOrderProblem OrderStats)
tryToExecuteBuyOrder mtgoxHandles safetyMarginUSD amountBTC amountUSD = runEitherT $ do
    privateInfo <- noteT (MtGoxCallError "Unable to call getPrivateInfoR.")
                    . MaybeT $ callHTTPApi mtgoxHandles getPrivateInfoR
    _ <- tryAssert MtGoxLowBalance
            (piUsdBalance privateInfo >= amountUSD + safetyMarginUSD) ()
    orderStats <- EitherT $
        adjustMtGoxError <$> callHTTPApi mtgoxHandles submitOrder
                                OrderTypeBuyBTC amountBTC
    return orderStats

adjustMtGoxError (Left err) = Left (MtGoxCallError err)
adjustMtGoxError (Right result) = Right result

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
