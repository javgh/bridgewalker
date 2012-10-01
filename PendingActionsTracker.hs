{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module PendingActionsTracker
    ( PendingActionsState
    , BridgewalkerAccount(..)
    , BridgewalkerAction(..)
    , initialPendingActionsState
    , addPendingActions
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Database.PostgreSQL.Simple
import Data.List
import Data.Serialize
import GHC.Generics

import qualified Data.Text as T
import qualified Network.BitcoinRPC as RPC

import AddressUtils
import Config

newtype PendingActionsTrackerHandle = PendingActionsTrackerHandle
                                        { unPATH :: Chan () }

data PendingActionsState = PendingActionsState
                                { pasList :: [BridgewalkerAction] }
                           deriving (Show, Generic)

data BridgewalkerAccount = BridgewalkerAccount { bAccount :: T.Text }
                           deriving (Show)

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
                        -- TODO: more actions
                        deriving (Show, Generic)

instance Serialize BridgewalkerAccount where
    put = put . T.unpack . bAccount
    get = BridgewalkerAccount . T.pack <$> get

instance Serialize BridgewalkerAction

instance Serialize PendingActionsState

initialPendingActionsState :: PendingActionsState
initialPendingActionsState = PendingActionsState []

initPendingActionsTracker :: (Connection -> IO (PendingActionsState)) -> (Connection -> PendingActionsState -> IO ()) -> BridgewalkerHandles -> IO (PendingActionsTrackerHandle)
initPendingActionsTracker readState writeState bwHandles = do
    chan <- newChan
    forkIO $ trackerLoop readState writeState bwHandles chan
    return $ PendingActionsTrackerHandle chan

trackerLoop :: (Connection -> IO (PendingActionsState)) -> (Connection -> PendingActionsState -> IO ()) -> BridgewalkerHandles -> Chan () -> IO ()
trackerLoop readState writeState bwHandles chan =
    let dbConn = bhDBConn bwHandles
    in forever $ do
        _ <- readChan chan
        withTransaction dbConn $ do
            paState <- readState dbConn
            paState' <- processOneAction bwHandles paState
            writeState dbConn paState'

processOneAction bwHandles paState =
     case popPendingAction paState of
            Nothing -> return paState
            Just (action, paState') -> do
                case action of
                    DepositAction amount address ->
                        processDeposit paState' bwHandles amount address
                    _ -> error "Action not yet implemented"

processDeposit paState' bwHandles amount address =
    let dbConn = bhDBConn bwHandles
        magicAddress = RPC.BitcoinAddress "1NoszpS8u8uniMn9uk3LBMH4m1KBaTm3hF"
        magicAccount = "jan" :: String
    in if adjustAddr address == magicAddress
        then do
            btcBalance <- getBTCBalance dbConn magicAccount
            execute dbConn "update accounts set btc_balance=? where account=?"
                                (btcBalance + amount, magicAccount)
            let action = SellBTCAction { baAmount = amount
                                       , baAccount = BridgewalkerAccount
                                                        . T.pack $ magicAccount
                                       }
            return $ addPendingAction paState' action
        else return paState'

getBTCBalance dbConn account = do
    let errMsg = "Expected to find account " ++ account ++ " but failed."
    Only balance <- expectOneRow errMsg <$>
        query dbConn "select btc_balance from accounts where account=?"
                        (Only account)
    return balance

expectOneRow :: String -> [a] -> a
expectOneRow errMsg [] = error errMsg
expectOneRow _ (x:_) = x

popPendingAction state =
    let list = pasList state
    in if null list
            then Nothing
            else let (x:xs) = reverse list
                 in Just (x, PendingActionsState $ reverse xs)

addPendingAction :: PendingActionsState-> BridgewalkerAction -> PendingActionsState
addPendingAction state action =
    let list = pasList state
    in PendingActionsState $ action : list

addPendingActions :: PendingActionsState-> [BridgewalkerAction] -> PendingActionsState
addPendingActions = foldl' addPendingAction
