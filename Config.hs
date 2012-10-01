module Config
    ( readConfig
    , BridgewalkerConfig(..)
    , BridgewalkerHandles(..)
    ) where

import Control.Applicative
import Control.Monad.Error
import Database.PostgreSQL.Simple
import Data.ConfigFile
import Network.BitcoinRPC
import Network.BitcoinRPC.Events.MarkerAddresses
import Network.MtGoxAPI hiding (BitcoinAddress)
import System.Environment
import System.FilePath

import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T

import LoggingUtils
import Rebalancer
import ScrambleCredentials

data BridgewalkerConfig = BridgewalkerConfig
                            { bcRPCAuth :: !RPCAuth
                            , bcMtGoxCredentials :: !MtGoxCredentials
                            , bcSafetyMargin :: !Integer
                            , bcNotifyFile :: !FilePath
                            , bcMarkerAddresses :: ![(BitcoinAddress, BitcoinAmount)]
                            }

data BridgewalkerHandles = BridgewalkerHandles
                            { bhAppLogger :: Logger
                            , bhConfig :: BridgewalkerConfig
                            , bhDBConn :: Connection
                            , bhMtGoxHandles :: MtGoxAPIHandles
                            , bhFilteredBitcoinEventTaskHandle :: FilteredBitcoinEventTaskHandle
                            , bhRebalancerHandle :: RebalancerHandle
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
            safetyMarginF <- get cp "DEFAULT" "safety_margin"
            notifyFile <- get cp "DEFAULT" "bitcoind_notify_file"
            markerAddresses <- get cp "DEFAULT" "marker_addresses"
            let authKey = B8.pack $
                            unScrambleText authKeyScrambled hardcodedKeyA
                authSecret = B8.pack $
                                unScrambleText authSecretScrambled hardcodedKeyB
                safetyMargin = round $ (safetyMarginF :: Double) * 10 ^ (8 :: Integer)
            return $ BridgewalkerConfig
                        { bcRPCAuth = rpcAuth
                        , bcMtGoxCredentials = initMtGoxCredentials authKey authSecret
                        , bcSafetyMargin = safetyMargin
                        , bcNotifyFile = notifyFile
                        , bcMarkerAddresses = adjustMarkerAddresses (read markerAddresses)
                        }
    case v of
        Left msg -> error $ "Reading the configuration failed " ++ show msg
        Right cfg -> return cfg

adjustMarkerAddresses :: [(T.Text, Integer)] -> [(BitcoinAddress, BitcoinAmount)]
adjustMarkerAddresses = map go
  where
    go (addr, amount) = (BitcoinAddress addr,
                            BitcoinAmount (amount * 10 ^ (8 :: Integer)))
