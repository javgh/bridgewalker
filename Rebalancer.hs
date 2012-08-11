module Rebalancer
    ( runRebalancer
    , decideRebalance
    , RebalancerLog(..)
    , RebalancerAction(..)
    ) where

import Control.Applicative
import Control.Error
import Control.Monad.IO.Class
import Data.Maybe
import Network.BitcoinRPC hiding (BitcoinAddress)
import Network.MtGoxAPI hiding (BitcoinAddress)
import System.Environment
import System.FilePath

import qualified Network.BitcoinRPC as RPC
import qualified Network.MtGoxAPI as MtGox

import qualified Control.Exception as E

smallestCoin :: Integer
smallestCoin = 1000000

confsNeededForSending :: Integer
confsNeededForSending = 3

data RebalancerLog = NothingDoTo
                   | WillRebalance Integer
                   | UnableToRebalance Integer
                   deriving (Eq, Show)

data RebalancerAction = DoNothing
                      | MtGoxToBitcoind Integer
                      | BitcoindToMtGox Integer
                      deriving (Eq, Show)

tryIO' :: IO a -> IO (Either E.IOException a)
tryIO' = E.try

maybeRead :: String -> Maybe Double
maybeRead = fmap fst . listToMaybe . reads

getConfFile home = home </> ".bridgewalker/workingfund"

readTargetBalance :: IO (Maybe Integer)
readTargetBalance = do
    confFile <- getConfFile <$> getEnv "HOME"
    contentM <- tryIO' $ readFile confFile
    return $ case contentM of
        Left _ -> Nothing
        Right content -> fromDouble <$> maybeRead content
  where
    fromDouble d = round $ d * 10 ^ (8 :: Integer)

-- TODO: improve logging
-- TODO: make sure that function is safe to call twice and will
--       not take action too often
-- TODO: think about BitcoinAddress problematic ->
--          maybe combined type module really needed

runRebalancer :: Maybe WatchdogLogger-> RPCAuth -> MtGoxAPIHandles -> IO (Maybe RebalancerLog)
runRebalancer mLogger rpcAuth mtgoxHandles = do
    values <- runMaybeT $ do
        tB <- MaybeT $ readTargetBalance
        zCB <- liftIO $ btcAmount <$> getBalanceR mLogger rpcAuth 0 True
        eCB <- liftIO $ btcAmount <$> getBalanceR mLogger rpcAuth
                                                    confsNeededForSending True
        mB <- MaybeT $ fmap piBtcBalance <$>
                                callHTTPApi mtgoxHandles getPrivateInfoR
        return (tB, zCB, eCB, mB)
    case values of
        Nothing -> return Nothing
        Just (tB, zCB, eCB, mB) -> do
            let (log, action) = decideRebalance tB zCB eCB mB
            case action of
                DoNothing -> return ()
                MtGoxToBitcoind v -> mtgoxToBitcoind v
                BitcoindToMtGox v -> bitcoindToMtgox v
            return $ Just log
  where
    mtgoxToBitcoind v = do
        addrT <- btcAddress <$> getNewAddressR Nothing rpcAuth
        (callHTTPApi' mtgoxHandles withdrawBitcoins)
                            (MtGox.BitcoinAddress addrT) v
        return ()
    bitcoindToMtgox v = do
        addrM <- callHTTPApi mtgoxHandles getBitcoinDepositAddressR
        case addrM of
            Nothing -> return ()
            Just addr -> do
                let addrT = baAddress . bdaAddr $ addr
                sendToAddress rpcAuth
                    (RPC.BitcoinAddress addrT) (BitcoinAmount v)
                return ()

decideRebalance :: Integer-> Integer-> Integer-> Integer-> (RebalancerLog, RebalancerAction)
decideRebalance targetBalance zeroConfBalance enoughConfBalance mtgoxBalance =
    let stepSizeM = findStepSize targetBalance
        diff = abs $ zeroConfBalance - targetBalance
    in case stepSizeM of
        Nothing -> (NothingDoTo, DoNothing)
        Just stepSize ->
            let diffLevel = diff `div` stepSize
            in if diffLevel == 0
                then (NothingDoTo, DoNothing)
                else let transfer = zeroConfBalance - targetBalance
                     in decideTransfer diffLevel transfer
  where
    decideTransfer diffLevel transfer
      | transfer == 0 = (NothingDoTo, DoNothing)    -- shouldn't really end up here,
                                                    -- but just for completeness
      | transfer > 0 =
        let transfer' = min enoughConfBalance transfer
        in if transfer' >= smallestCoin
            then (WillRebalance diffLevel, BitcoindToMtGox transfer')
            else (UnableToRebalance diffLevel, DoNothing)
      | transfer < 0 =
        let transfer' = min mtgoxBalance (-1 * transfer)
        in if transfer' >= smallestCoin
            then (WillRebalance diffLevel, MtGoxToBitcoind transfer')
            else (UnableToRebalance diffLevel, DoNothing)

findStepSize :: Integer -> Maybe Integer
findStepSize targetBalance =
    let stepSize = targetBalance `div` 4
        rounded = stepSize - stepSize `mod` smallestCoin
    in if rounded >= smallestCoin
        then Just rounded
        else Nothing
