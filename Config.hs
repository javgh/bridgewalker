module Config
    ( readConfig
    ) where

import Control.Applicative
import Control.Monad.Error
import Data.ConfigFile
import Network.BitcoinRPC
import Network.MtGoxAPI
import System.Environment
import System.FilePath

import qualified Data.ByteString.Char8 as B8

import ScrambleCredentials

getConfFile home = home </> ".bridgewalker/config"

readConfig :: IO (RPCAuth, MtGoxCredentials)
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
            let authKey = B8.pack $
                            unScrambleText authKeyScrambled hardcodedKeyA
                authSecret = B8.pack $
                                unScrambleText authSecretScrambled hardcodedKeyB
            return (rpcAuth, initMtGoxCredentials authKey authSecret)
    case v of
        Left msg -> error $ "Reading the configuration failed " ++ show msg
        Right cfg -> return cfg
