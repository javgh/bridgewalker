{-# LANGUAGE OverloadedStrings #-}
module Prototype where

import Control.Applicative
import Control.Concurrent
import Control.Error
import Control.Monad
import Database.PostgreSQL.Simple
import Data.Serialize
import Network.BitcoinRPC
import Network.BitcoinRPC.Events
import Network.BitcoinRPC.MarkerAddresses
import Network.MtGoxAPI

import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64

import AddressUtils
import Config
import Rebalancer
import LoggingUtils

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

writeBitcoindStateToDB :: Connection -> EventTaskState -> MAStore -> IO ()
writeBitcoindStateToDB conn etState maStore = do
    let etStateStr = B64.encode . encode $ etState
        maStoreStr = B64.encode . encode $ maStore
    writeStateToDB conn "eventtaskstate" etStateStr
    writeStateToDB conn "mastore" maStoreStr
    return ()

readBitcoindStateFromDB :: Connection -> IO (EventTaskState, MAStore)
readBitcoindStateFromDB conn = do
    etStateStr <- readStateFromDB conn "eventtaskstate"
    maStoreStr <- readStateFromDB conn "mastore"
    let etState = B64.decode etStateStr >>= decode
        maStore = B64.decode maStoreStr >>= decode
    return (expectRight etState, expectRight maStore)
  where
    expectRight (Right r) = r
    expectRight (Left msg) = error msg

--main = connectPostgreSQL myConnectInfo >>= \conn -> do
--    (etS, maS) <- readBitcoindStateFromDB conn
--    writeBitcoindStateToDB conn etS maS

acceptAfterThreeConfs txHeader = thConfirmations txHeader >= 3

justCatchUp :: Connection-> Chan (EventTaskState, [BitcoinEvent]) -> MAStore -> IO ()
justCatchUp dbConn betChan maStore = go maStore
  where
    go maStore' = do
        (etState', events) <- readChan betChan
        let (maStore'', _) = processEvents maStore' events
        writeBitcoindStateToDB dbConn etState' maStore''
        go maStore''

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

actOnDeposits :: Connection-> Chan (EventTaskState, [BitcoinEvent])-> MAStore-> MtGoxAPIHandles-> Integer-> IO ()
actOnDeposits dbConn betChan maStore mtgoxHandles safetyMargin = go maStore
  where
    go maStore' = do
        (etState', events) <- readChan betChan
        let (maStore'', fevents) = processEvents maStore' events
        writeBitcoindStateToDB dbConn etState' maStore''
        mapM_ act fevents
        go maStore''
    act fevent = case fevent of
                    fTx@FilteredNewTransaction{} -> do
                        let amount = tAmount . fntTx $ fTx
                        sell <- tryToSellBtc mtgoxHandles safetyMargin amount
                        case sell of
                            Left msg -> putStrLn msg
                            Right stats -> displayStats stats
                    _ -> return ()

periodicRebalancing :: RebalancerHandle -> IO ()
periodicRebalancing rbHandle = forever $ do
    runRebalancer rbHandle
    threadDelay $ 5 * 10 ^ (6 :: Integer)

main :: IO ()
main = do
    dbConn <- connectPostgreSQL myConnectInfo
    (etState, maStore) <- readBitcoindStateFromDB dbConn
    (rpcAuth, mtgoxCreds, safetyMargin, bitcoindNotifyFile, markerAddresses)
        <- readConfig
    appLogger <- initLogger
    let maStore' = updateMarkerAddresses maStore markerAddresses
    betChan <- newChan
    mtgoxHandles <- initMtGoxAPI Nothing mtgoxCreds
    _ <- forkIO $ bitcoinEventTask Nothing rpcAuth bitcoindNotifyFile
                    acceptAfterThreeConfs etState betChan
    rbHandle <- initRebalancer appLogger Nothing
                                    rpcAuth mtgoxHandles safetyMargin
    --justCatchUp dbConn betChan maStore
    _ <- forkIO $ periodicRebalancing rbHandle
    actOnDeposits dbConn betChan maStore' mtgoxHandles safetyMargin
    return ()

-- TODO: make processEvents from MarkerAddresses easier to use;
--       -> wrap bitcoinEventTask and processEvents to create one single loop
--       also: switch to handle structure and replace 'readChan' with a custom
--       'getNewEvents' or something like that
--
-- TODO: Start PendingBridgeWalkerActions infrastructure to do things like
--         selling BTC as soon as it is possible
--
-- TODO: Refactor config module; probably use some kind of app-wide config
--       datatype
--
-- TODO: Figure out better way to pass all the various handlers around
