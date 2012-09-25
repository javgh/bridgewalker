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
import Rebalancer
import LoggingUtils

data BridgewalkerHandles = BridgewalkerHandles
                            { bhAppLogger :: Logger
                            , bhConfig :: BridgewalkerConfig
                            , bhDBConn :: Connection
                            , bhMtGoxHandles :: MtGoxAPIHandles
                            , bhFilteredBitcoinEventTaskHandle :: FilteredBitcoinEventTaskHandle
                            , bhRebalancerHandle :: RebalancerHandle
                            }


myConnectInfo :: B.ByteString
myConnectInfo = "dbname=bridgewalker"

readStateFromDB :: Connection -> B.ByteString -> IO B.ByteString
readStateFromDB conn key = do
    Only stateStr <- getFirstRow <$>
        query conn "select state from bitcoind where key=?" (Only key)
    return stateStr
  where
    getFirstRow [] = error "Expected at least one row, but got none."
    getFirstRow (x:_) = x

writeStateToDB :: Connection -> B.ByteString -> B.ByteString -> IO ()
writeStateToDB conn key stateStr = do
    execute conn "update bitcoind set state=? where key=?" (stateStr, key)
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
  where
    expectRight (Right r) = r
    expectRight (Left msg) = error msg

acceptAfterThreeConfs txHeader = thConfirmations txHeader >= 3

periodicRebalancing :: RebalancerHandle -> IO ()
periodicRebalancing rbHandle = forever $ do
    runRebalancer rbHandle
    threadDelay $ 5 * 10 ^ (6 :: Integer)

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
                                    mtgoxHandles (bcSafetyMargin bwConfig)
    _ <- forkIO $ periodicRebalancing rbHandle
    return $ BridgewalkerHandles { bhAppLogger = appLogger
                                 , bhConfig = bwConfig
                                 , bhDBConn = dbConn
                                 , bhMtGoxHandles = mtgoxHandles
                                 , bhFilteredBitcoinEventTaskHandle = fbetHandle
                                 , bhRebalancerHandle = rbHandle
                                 }

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

actOnDeposits :: BridgewalkerHandles -> IO ()
actOnDeposits bwHandles =
    let fbetHandle = bhFilteredBitcoinEventTaskHandle bwHandles
        dbConn = bhDBConn bwHandles
        mtgoxHandles = bhMtGoxHandles bwHandles
        safetyMargin = bcSafetyMargin . bhConfig $ bwHandles
    in forever $ do
        (fetState, fEvents) <- waitForFilteredBitcoinEvents fbetHandle
        writeBitcoindStateToDB dbConn fetState
        mapM_ (act mtgoxHandles safetyMargin) fEvents
  where
    act mtgoxHandles safetyMargin fEvent =
            case fEvent of
                fTx@FilteredNewTransaction{} -> do
                    let amount = tAmount . fntTx $ fTx
                    sell <- tryToSellBtc mtgoxHandles safetyMargin amount
                    case sell of
                        Left msg -> putStrLn msg
                        Right stats -> displayStats stats
                _ -> return ()

main :: IO ()
main = do
    bwHandles <- initBridgewalkerHandles myConnectInfo
    --justCatchUp bwHandles
    actOnDeposits bwHandles

-- TODO: Start PendingBridgeWalkerActions infrastructure to do things like
--         selling BTC as soon as it is possible
--
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
