{-# LANGUAGE OverloadedStrings #-}
module Prototype where

import Control.Applicative
import Control.Concurrent
import Control.Error
import Control.Monad
import Database.PostgreSQL.Simple
import Data.Serialize
import Network.BitcoinRPC
import Network.BitcoinRPC.Events.MarkerAddresses
import Network.MtGoxAPI

import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64

import AddressUtils
import Config
import ConfigTypes
import Rebalancer
import LoggingUtils
import PendingActionsTracker

import qualified PendingActionsTracker as PAT

myConnectInfo :: B.ByteString
myConnectInfo = "dbname=bridgewalker"

readStateFromDB :: Connection -> B.ByteString -> IO B.ByteString
readStateFromDB conn key = do
    Only stateStr <- getFirstRow <$>
        query conn "select state from states where key=?" (Only key)
    return stateStr
  where
    getFirstRow [] = error "Expected at least one row, but got none."
    getFirstRow (x:_) = x

writeStateToDB :: Connection -> B.ByteString -> B.ByteString -> IO ()
writeStateToDB conn key stateStr = do
    execute conn "update states set state=? where key=?" (stateStr, key)
    return ()

writeBitcoindStateToDB :: Connection -> FilteredEventTaskState -> IO ()
writeBitcoindStateToDB conn fetState = do
    let fetStateStr = B64.encode . encode $ fetState
    writeStateToDB conn "filteredeventtaskstate" fetStateStr
    return ()

readBitcoindStateFromDB :: Connection -> IO (FilteredEventTaskState)
readBitcoindStateFromDB conn = do
    fetStateStr <- readStateFromDB conn "filteredeventtaskstate"
    let fetState = B64.decode fetStateStr >>= decode
    return $ expectRight fetState

writePendingActionsStateToDB :: Connection -> PendingActionsState -> IO ()
writePendingActionsStateToDB conn paState = do
    let paStateStr = B64.encode . encode $ paState
    writeStateToDB conn "pendingactionsstate" paStateStr
    return ()

readPendingActionsStateFromDB :: Connection -> IO (PendingActionsState)
readPendingActionsStateFromDB conn = do
    paStateStr <- readStateFromDB conn "pendingactionsstate"
    let paState = B64.decode paStateStr >>= decode
    return $ expectRight paState

expectRight :: Either String t -> t
expectRight (Right r) = r
expectRight (Left msg) = error msg

acceptAfterThreeConfs txHeader = thConfirmations txHeader >= 3

periodicRebalancing :: RebalancerHandle -> IO ()
periodicRebalancing rbHandle = forever $ do
    runRebalancer rbHandle
    threadDelay $ 5 * 10 ^ (6 :: Integer)

periodicNudging :: PendingActionsTrackerHandle -> IO ()
periodicNudging patHandle = forever $ do
    nudgePendingActionsTracker patHandle
    threadDelay $ 60 * 10 ^ (6 :: Integer)

initBridgewalkerHandles :: B.ByteString -> IO BridgewalkerHandles
initBridgewalkerHandles connectInfo = do
    appLogger <- initLogger
    bwConfig <- readConfig
    let maConfig = bcMarkerAddresses bwConfig
    dbConn <- connectPostgreSQL connectInfo
    fetState <- readBitcoindStateFromDB dbConn >>= \s
                    -> return $ updateMarkerAddresses s maConfig
    mtgoxHandles <- initMtGoxAPI Nothing (bcMtGoxCredentials bwConfig)
    fbetHandle <- initFilteredBitcoinEventTask Nothing (bcRPCAuth bwConfig)
                    (bcNotifyFile bwConfig) acceptAfterThreeConfs fetState
    rbHandle <- initRebalancer appLogger Nothing (bcRPCAuth bwConfig)
                                    mtgoxHandles (bcSafetyMarginBTC bwConfig)
    _ <- forkIO $ periodicRebalancing rbHandle
    let tempBWHandles =
            BridgewalkerHandles { bhAppLogger = appLogger
                                , bhConfig = bwConfig
                                , bhDBConn = dbConn
                                , bhMtGoxHandles = mtgoxHandles
                                , bhFilteredBitcoinEventTaskHandle = fbetHandle
                                , bhRebalancerHandle = rbHandle
                                , bhPendingActionsTrackerHandle =
                                    error "PendingActionsTracker was accessed,\
                                          \ but not initialized yet."
                                }
    patHandle <- initPendingActionsTracker readPendingActionsStateFromDB
                    writePendingActionsStateToDB tempBWHandles
    _ <- forkIO $ periodicNudging patHandle
    return $ tempBWHandles { bhPendingActionsTrackerHandle = patHandle }

justCatchUp :: BridgewalkerHandles -> IO ()
justCatchUp bwHandles =
    let fbetHandle = bhFilteredBitcoinEventTaskHandle bwHandles
        dbConn = bhDBConn bwHandles
    in forever $ do
        (fetState, _) <- waitForFilteredBitcoinEvents fbetHandle
        writeBitcoindStateToDB dbConn fetState

tryToSellBtc :: MtGoxAPIHandles -> Integer -> BitcoinAmount -> IO (Either String OrderStats)
tryToSellBtc mtgoxHandles safetyMargin amount = runEitherT $ do
    privateInfo <- noteT "Unable to call getPrivateInfoR"
                    . MaybeT $ callHTTPApi mtgoxHandles getPrivateInfoR
    _ <- tryAssert "Not enough funds available at MtGox to sell BTC"
            (piBtcBalance privateInfo >= adjustAmount amount + safetyMargin) ()
    orderStats <- EitherT $
        callHTTPApi mtgoxHandles submitOrder
            OrderTypeSellBTC (adjustAmount amount)
    return orderStats

displayStats :: OrderStats -> IO ()
displayStats stats =
    let usd = fromIntegral (usdEarned stats) / (10 ^ (5 :: Integer))
    in putStrLn $ "Account activity: + $" ++ show usd

--actOnDeposits :: BridgewalkerHandles -> IO ()
--actOnDeposits bwHandles =
--    let fbetHandle = bhFilteredBitcoinEventTaskHandle bwHandles
--        dbConn = bhDBConn bwHandles
--        mtgoxHandles = bhMtGoxHandles bwHandles
--        safetyMargin = bcSafetyMargin . bhConfig $ bwHandles
--    in forever $ do
--        (fetState, fEvents) <- waitForFilteredBitcoinEvents fbetHandle
--        writeBitcoindStateToDB dbConn fetState
--        mapM_ (act mtgoxHandles safetyMargin) fEvents
--  where
--    act mtgoxHandles safetyMargin fEvent =
--            case fEvent of
--                fTx@FilteredNewTransaction{} -> do
--                    let amount = tAmount . fntTx $ fTx
--                    sell <- tryToSellBtc mtgoxHandles safetyMargin amount
--                    case sell of
--                        Left msg -> putStrLn msg
--                        Right stats -> displayStats stats
--                _ -> return ()

actOnDeposits :: BridgewalkerHandles -> IO ()
actOnDeposits bwHandles =
    let fbetHandle = bhFilteredBitcoinEventTaskHandle bwHandles
        dbConn = bhDBConn bwHandles
        patHandle = bhPendingActionsTrackerHandle bwHandles
    in forever $ do
        (fetState, fEvents) <- waitForFilteredBitcoinEvents fbetHandle
        let actions = concatMap convertToActions fEvents
        withTransaction dbConn $ do     -- atomic transaction: do not update
                                        -- fetState, before recording necessary
                                        -- actions to be done as a result
            paState <- readPendingActionsStateFromDB dbConn
            let paState' = addPendingActions paState actions
            writeBitcoindStateToDB dbConn fetState
            writePendingActionsStateToDB dbConn paState'
        nudgePendingActionsTracker patHandle
  where
    convertToActions fTx@FilteredNewTransaction{} =
        let amount = adjustAmount . tAmount . fntTx $ fTx
            address = tAddress . fntTx $ fTx
        in [DepositAction { baAmount = amount, PAT.baAddress = address }]
    convertToActions _ = []

main :: IO ()
main = do
    bwHandles <- initBridgewalkerHandles myConnectInfo
    --justCatchUp bwHandles
    actOnDeposits bwHandles

-- TODO: Find bug - either: something related to standard transactions
--                      or: something related to marker transactions, that
--                            confirm while the application is not running
--                            (update: seems not to be the case)
--                      or: a combination of these (?)
--                      ---> try to design a unit test that involves shutting
--                      down and restarting from database after each step
--
--main = connectPostgreSQL myConnectInfo >>= \conn -> do
--    fetS <- readBitcoindStateFromDB conn
--    writeBitcoindStateToDB conn fetS
