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
import Network.MtGoxAPI hiding (BitcoinAddress)
import System.Environment
import System.FilePath

import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T

import CommonTypes
import LoggingUtils
import Rebalancer
import ScrambleCredentials

data BridgewalkerConfig = BridgewalkerConfig
                            { bcRPCAuth :: !RPCAuth
                            , bcMtGoxCredentials :: !MtGoxCredentials
                            , bcSafetyMarginBTC :: !Integer
                            , bcSafetyMarginUSD :: !Integer
                            , bcMaximalOrderBTC :: !Integer
                            , bcMtGoxMinimumOrderBTC :: !Integer
                            , bcTargetExchangeFee :: !Double
                            , bcNotifyFile :: !FilePath
                            , bcMarkerAddresses
                                :: ![(BitcoinAddress, BitcoinAmount)]
                            }

data BridgewalkerHandles = BridgewalkerHandles
                            { bhLoggingHandle :: LoggingHandle
                            , bhAppLogger :: Logger
                            , bhWatchdogLogger :: WatchdogLogger
                            , bhConfig :: BridgewalkerConfig
                            , bhDBConnPAT :: Connection
                            , bhDBConnCH :: Connection
                            , bhDBConnFBET :: Connection
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

getConfFile home = home </> ".bridgewalker/config"

readConfig :: IO (BridgewalkerConfig)
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
            safetyMarginBTCF <- get cp "DEFAULT" "safety_margin_btc"
            safetyMarginUSDF <- get cp "DEFAULT" "safety_margin_usd"
            maximalOrderBTCF <- get cp "DEFAULT" "maximal_order_btc"
            mtgoxMinimumOrderBTCF <- get cp "DEFAULT" "mtgox_minimum_order_btc"
            targetExchangeFee <- get cp "DEFAULT" "target_exchange_fee"
            notifyFile <- get cp "DEFAULT" "bitcoind_notify_file"
            markerAddresses <- get cp "DEFAULT" "marker_addresses"
            let authKey = B8.pack $
                            unScrambleText authKeyScrambled hardcodedKeyA
                authSecret = B8.pack $
                                unScrambleText authSecretScrambled hardcodedKeyB
                safetyMarginBTC =
                    round $ (safetyMarginBTCF :: Double) * 10 ^ (8 :: Integer)
                safetyMarginUSD =
                    round $ (safetyMarginUSDF :: Double) * 10 ^ (5 :: Integer)
                maximalOrderBTC =
                    round $ (maximalOrderBTCF :: Double) * 10 ^ (8 :: Integer)
                mtgoxMinimumOrderBTC =
                    round $ (mtgoxMinimumOrderBTCF :: Double)
                                * 10 ^ (8 :: Integer)
            workingFund <- liftIO $ readTargetBalance
            case workingFund of
                Nothing ->
                    throwError (OtherProblem "Unable to read size\
                                             \ of working fund.", "")
                Just wf ->
                    when (safetyMarginBTC + maximalOrderBTC > wf) $
                        throwError (OtherProblem "Working fund not large\
                                                 \ enough to deal with\
                                                 \ largest possible order."
                                                 , "")
            return $ BridgewalkerConfig
                        { bcRPCAuth = rpcAuth
                        , bcMtGoxCredentials =
                            initMtGoxCredentials authKey authSecret
                        , bcSafetyMarginBTC = safetyMarginBTC
                        , bcSafetyMarginUSD = safetyMarginUSD
                        , bcMaximalOrderBTC = maximalOrderBTC
                        , bcMtGoxMinimumOrderBTC = mtgoxMinimumOrderBTC
                        , bcTargetExchangeFee = targetExchangeFee
                        , bcNotifyFile = notifyFile
                        , bcMarkerAddresses =
                            adjustMarkerAddresses (read markerAddresses)
                        }
    case v of
        Left msg -> error $ "Reading the configuration failed " ++ show msg
        Right cfg -> return cfg

adjustMarkerAddresses :: [(T.Text, Integer)] -> [(BitcoinAddress, BitcoinAmount)]
adjustMarkerAddresses = map go
  where
    go (addr, amount) = (BitcoinAddress addr,
                            BitcoinAmount (amount * 10 ^ (8 :: Integer)))
