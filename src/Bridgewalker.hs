{-# LANGUAGE OverloadedStrings #-}
module Bridgewalker
    ( initBridgewalker
    ) where

import Control.Concurrent
import Control.Monad
import Database.PostgreSQL.Simple
import Network.BitcoinRPC
import Network.BitcoinRPC.Events.MarkerAddresses
import Network.Metricsd.Client
import Network.MtGoxAPI
import System.Exit

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

import AddressUtils
import ClientHub
import CommonTypes
import Config
import DbUtils
import LoggingUtils
import PendingActionsTracker
import Rebalancer

import qualified CommonTypes as CT
import qualified Control.Exception as E

myConnectInfo :: B.ByteString
myConnectInfo = "dbname=bridgewalker"

acceptAfterThreeConfs :: TransactionHeader -> Bool
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
    (lHandle, appLogger) <- initLogging
    let watchdogLogger = adapt appLogger
    mcHandle <- initMetricsdClient
    bwConfig <- readConfig
    let maConfig = bcMarkerAddresses bwConfig
    dbConn1 <- connectPostgreSQL connectInfo
    dbConn2 <- connectPostgreSQL connectInfo
    dbConn3 <- connectPostgreSQL connectInfo
    dbWriteLock <- newMVar ()
    fetState <- readBitcoindStateFromDB dbConn1 >>= \s
                    -> return $ updateMarkerAddresses s maConfig
    let streamSettings = MtGoxStreamSettings
                            DisableWalletNotifications SkipFullDepth
    mtgoxHandles <- initMtGoxAPI (Just watchdogLogger)
                                    (bcMtGoxCredentials bwConfig)
                                    streamSettings
    mtgoxFee <- do
        privateInfoM <- callHTTPApi mtgoxHandles getPrivateInfoR
        case privateInfoM of
            Nothing -> error "Unable to determine current Mt.Gox fee"
            Just privateInfo -> return $ piFee privateInfo
    fetStateCopy <- newMVar fetState
    fbetHandle <- initFilteredBitcoinEventTask (Just watchdogLogger)
                    (bcRPCAuth bwConfig) (bcNotifyFile bwConfig)
                    acceptAfterThreeConfs fetState
    rbHandle <- initRebalancer appLogger (Just watchdogLogger) mcHandle
                                    (bcRPCAuth bwConfig) mtgoxHandles
                                    (bcSafetyMarginBTC bwConfig)
    patHandleMVar <- newEmptyMVar
    _ <- forkIO $ periodicRebalancing rbHandle
    let preliminaryBWHandles =
            BridgewalkerHandles { bhLoggingHandle = lHandle
                                , bhAppLogger = appLogger
                                , bhWatchdogLogger = watchdogLogger
                                , bhMetricsdClient = mcHandle
                                , bhConfig = bwConfig
                                , bhDBConnPAT = dbConn1
                                , bhDBConnCH = dbConn2
                                , bhDBConnFBET = dbConn3
                                , bhDBWriteLock = dbWriteLock
                                , bhMtGoxHandles = mtgoxHandles
                                , bhMtGoxFee = mtgoxFee
                                , bhFilteredBitcoinEventTaskHandle = fbetHandle
                                , bhFilteredEventStateCopy = fetStateCopy
                                , bhRebalancerHandle = rbHandle
                                , bhClientHubHandle =
                                    error "ClientHub was accessed,\
                                          \ but not initialized yet."
                                , bhPendingActionsTrackerHandleMVar
                                    = patHandleMVar
                                }
    chHandle <- initClientHub preliminaryBWHandles
    let bwHandles = preliminaryBWHandles { bhClientHubHandle = chHandle }
    patHandle <- initPendingActionsTracker bwHandles
    putMVar patHandleMVar patHandle
    _ <- forkIO $ periodicNudging patHandle
    return bwHandles
  where
    adapt :: Logger -> WatchdogLogger
    adapt logger taskErr delay =
        let logMsg = WatchdogError
                        { lcInfo = formatWatchdogError taskErr delay }
        in logger logMsg

--justCatchUp :: BridgewalkerHandles -> IO ()
--justCatchUp bwHandles =
--    let fbetHandle = bhFilteredBitcoinEventTaskHandle bwHandles
--        dbConn = bhDBConnFBET bwHandles
--        dbLock = bhDBWriteLock bwHandles
--    in forever $ do
--        (fetState, _) <- waitForFilteredBitcoinEvents fbetHandle
--        withSerialTransaction dbLock dbConn $
--            writeBitcoindStateToDB dbConn fetState

actOnDeposits :: BridgewalkerHandles -> IO ()
actOnDeposits bwHandles = do
    let fbetHandle = bhFilteredBitcoinEventTaskHandle bwHandles
        fetStateCopy = bhFilteredEventStateCopy bwHandles
        dbConn = bhDBConnFBET bwHandles
        dbLock = bhDBWriteLock bwHandles
        patHandleMVar = bhPendingActionsTrackerHandleMVar bwHandles
        chHandle = bhClientHubHandle bwHandles
    patHandle <- readMVar patHandleMVar
    forever $ do
        (fetState, fEvents) <- waitForFilteredBitcoinEvents fbetHandle
        let actions = concatMap convertToActions fEvents
        withSerialTransaction dbLock dbConn $ do
                        -- atomic transaction: do not update fetState, before
                        -- recording necessary actions to be done as a result
            addPendingActions dbConn actions
            writeBitcoindStateToDB dbConn fetState
            _ <- swapMVar fetStateCopy fetState
            return ()
        unless (null actions) $ nudgePendingActionsTracker patHandle
        signalPossibleBitcoinEvents chHandle
  where
    convertToActions fTx@FilteredNewTransaction{} =
        let amount = adjustAmount . tAmount . fntTx $ fTx
            address = tAddress . fntTx $ fTx
        in [DepositAction { baAmount = amount, CT.baAddress = address }]
    convertToActions _ = []

waitForDB :: IO ()
waitForDB = do
    r <- watchdog $ do
            setInitialDelay $ 1 * 10 ^ (6 :: Integer)      -- 1 second
            setMaximumRetries 8
            watchImpatiently $ do
                t <- tryDB
                return $ case t of
                            Left sq@SqlError {} ->
                                Left . B8.unpack . sqlErrorMsg $ sq
                            Right _ -> Right ()
    case r of
        Left _ -> do
            putStrLn "Giving up on database - exiting"
            exitFailure
        Right _ -> return ()
  where
    tryDB :: IO (Either SqlError ())
    tryDB = E.try $ connectPostgreSQL myConnectInfo >>= \conn -> close conn

initBridgewalker :: IO BridgewalkerHandles
initBridgewalker = do
    waitForDB
    bwHandles <- initBridgewalkerHandles myConnectInfo
    _ <- forkIO $ actOnDeposits bwHandles
    return bwHandles
