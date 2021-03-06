module Config
    ( readConfig
    , BridgewalkerConfig(..)
    , BridgewalkerHandles(..)
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad.Error
import Control.Watchdog
import Database.PostgreSQL.Simple
import Data.ConfigFile
import Network.BitcoinRPC
import Network.BitcoinRPC.Events.MarkerAddresses
import Network.Metricsd.Client
import Network.MtGoxAPI hiding (BitcoinAddress)
import System.Environment
import System.FilePath

import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T

import CommonTypes
import Rebalancer
import ScrambleCredentials

data BridgewalkerConfig = BridgewalkerConfig
                            { bcRPCAuth :: !RPCAuth
                            , bcMtGoxCredentials :: !MtGoxCredentials
                            , bcSafetyMarginBTC :: !Integer
                            , bcSafetyMarginUSD :: !Integer
                            , bcTypicalTxFee :: !Integer
                            , bcMaximumOrderBTC :: !Integer
                            , bcMtGoxMinimumOrderBTC :: !Integer
                            , bcTargetExchangeFee :: !Double
                            , bcNotifyFile :: !FilePath
                            , bcMarkerAddresses
                                :: ![(BitcoinAddress, BitcoinAmount)]
                            , bcStdOutLogging :: !Bool
                            }

data BridgewalkerHandles = BridgewalkerHandles
                            { bhLoggingHandle :: LoggingHandle
                            , bhAppLogger :: Logger
                            , bhMetricsdClient :: MetricsdClientHandle
                            , bhWatchdogLogger :: WatchdogLogger
                            , bhConfig :: BridgewalkerConfig
                            , bhDBConnPAT :: Connection
                            , bhDBConnCH :: Connection
                            , bhDBConnFBET :: Connection
                            , bhDBWriteLock :: MVar ()
                            , bhMtGoxHandles :: MtGoxAPIHandles
                            , bhMtGoxFee :: Double
                            , bhFilteredBitcoinEventTaskHandle
                                :: FilteredBitcoinEventTaskHandle
                            , bhFilteredEventStateCopy
                                :: MVar FilteredEventTaskState
                            , bhRebalancerHandle :: RebalancerHandle
                            , bhClientHubHandle :: ClientHubHandle
                            , bhPendingActionsTrackerHandleMVar
                                :: MVar PendingActionsTrackerHandle
                            }

getConfFile :: FilePath -> FilePath
getConfFile home = home </> ".bridgewalker/config"

readConfig :: IO BridgewalkerConfig
readConfig = do
    confFile <- getConfFile <$> getEnv "HOME"
    v <- runErrorT $ do
            cp <- join $ liftIO $ readfile emptyCP confFile
            url <- get cp "DEFAULT" "rpcurl"
            user <- get cp "DEFAULT" "rpcuser"
            password <- get cp "DEFAULT" "rpcpassword"
            let rpcAuth = RPCAuth url user password

            authKeyScrambled <- get cp "DEFAULT" "mtgox_auth_key"
            authSecretScrambled <- get cp "DEFAULT" "mtgox_auth_secret"
            keySet <- get cp "DEFAULT" "key_set"
            safetyMarginBTCF <- get cp "DEFAULT" "safety_margin_btc"
            safetyMarginUSDF <- get cp "DEFAULT" "safety_margin_usd"
            typicalTxFeeF <- get cp "DEFAULT" "typical_tx_fee"
            maximumOrderBTCF <- get cp "DEFAULT" "maximum_order_btc"
            mtgoxMinimumOrderBTCF <- get cp "DEFAULT" "mtgox_minimum_order_btc"
            targetExchangeFee <- get cp "DEFAULT" "target_exchange_fee"
            notifyFile <- get cp "DEFAULT" "bitcoind_notify_file"
            markerAddresses <- get cp "DEFAULT" "marker_addresses"
            stdOutLogging <- get cp "DEFAULT" "stdout_logging"
            let authKey = B8.pack $
                            unScrambleText authKeyScrambled
                                (case keySet :: Integer of
                                    1 -> hardcodedKeyA
                                    2 -> hardcodedKeyC
                                    _ -> error "Unknown keyset")
                authSecret = B8.pack $
                                unScrambleText authSecretScrambled
                                    (case keySet :: Integer of
                                        1 -> hardcodedKeyB
                                        2 -> hardcodedKeyD
                                        _ -> error "Unknown keyset")
                safetyMarginBTC =
                    round $ (safetyMarginBTCF :: Double) * 10 ^ (8 :: Integer)
                safetyMarginUSD =
                    round $ (safetyMarginUSDF :: Double) * 10 ^ (5 :: Integer)
                typicalTxFee =
                    round $ (typicalTxFeeF :: Double) * 10 ^ (8 :: Integer)
                maximumOrderBTC =
                    round $ (maximumOrderBTCF :: Double) * 10 ^ (8 :: Integer)
                mtgoxMinimumOrderBTC =
                    round $ (mtgoxMinimumOrderBTCF :: Double)
                                * 10 ^ (8 :: Integer)
            workingFund <- liftIO readTargetBalance
            case workingFund of
                Nothing ->
                    throwError (OtherProblem "Unable to read size\
                                             \ of working fund.", "")
                Just wf ->
                    when (safetyMarginBTC + maximumOrderBTC > wf) $
                        throwError (OtherProblem "Working fund not large\
                                                 \ enough to deal with\
                                                 \ largest possible order."
                                                 , "")
            when (maximumOrderBTC < mtgoxMinimumOrderBTC) $
                throwError (OtherProblem "Maximum BTC order needs to be at\
                                         \ least as large as Mt.Gox minimum\
                                         \ BTC order.", "")
            return BridgewalkerConfig
                      { bcRPCAuth = rpcAuth
                      , bcMtGoxCredentials =
                          initMtGoxCredentials authKey authSecret
                      , bcSafetyMarginBTC = safetyMarginBTC
                      , bcSafetyMarginUSD = safetyMarginUSD
                      , bcTypicalTxFee = typicalTxFee
                      , bcMaximumOrderBTC = maximumOrderBTC
                      , bcMtGoxMinimumOrderBTC = mtgoxMinimumOrderBTC
                      , bcTargetExchangeFee = targetExchangeFee
                      , bcNotifyFile = notifyFile
                      , bcMarkerAddresses =
                          adjustMarkerAddresses (read markerAddresses)
                      , bcStdOutLogging = stdOutLogging
                      }
    case v of
        Left msg -> error $ "Reading the configuration failed " ++ show msg
        Right cfg -> return cfg

adjustMarkerAddresses :: [(T.Text, Integer)] -> [(BitcoinAddress, BitcoinAmount)]
adjustMarkerAddresses = map go
  where
    go (addr, amount) = (BitcoinAddress addr,
                            BitcoinAmount (amount * 10 ^ (8 :: Integer)))
