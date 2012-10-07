module Rebalancer
    ( initRebalancer
    , runRebalancer
    , decideRebalance
    , RebalancerHandle
    , RebalancerLog(..)
    , RebalancerAction(..)
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Error
import Control.Monad.IO.Class
import Data.Maybe
import Data.Time
import Network.BitcoinRPC
import Network.MtGoxAPI
import System.Environment
import System.FilePath

import qualified Control.Exception as E

import AddressUtils
import LoggingUtils
import ConfigTypes

smallestCoin :: Integer
smallestCoin = 1000000

timeBetweenActions = 60     -- minimal time between rebalancer actions

data RebalancerLog = NothingDoTo
                   | WillRebalance Integer
                   | UnableToRebalance Integer
                   deriving (Eq, Show)

data RebalancerAction = DoNothing
                      | MtGoxToBitcoind Integer
                      | BitcoindToMtGox Integer
                      deriving (Eq, Show)

data RebalancerData = RebalancerData { rbTimestamp :: Maybe UTCTime
                                     , rbAppLogger :: Logger
                                     , rbWatchdogLogger :: Maybe WatchdogLogger
                                     , rbRPCAuth :: RPCAuth
                                     , rbMtGoxAPIHandles :: MtGoxAPIHandles
                                     , rbSafetyMargin :: Integer
                                     }

newtype RebalancerHandle = RebalancerHandle { unRH :: MVar RebalancerData }

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

initRebalancer :: Logger-> Maybe WatchdogLogger-> RPCAuth-> MtGoxAPIHandles-> Integer-> IO RebalancerHandle
initRebalancer appLogger mLogger rpcAuth mtgoxHandles safetyMargin =
    let rd = RebalancerData Nothing appLogger mLogger
                                rpcAuth mtgoxHandles safetyMargin
    in RebalancerHandle <$> newMVar rd

runRebalancer :: RebalancerHandle -> IO ()
runRebalancer rbHandle = do
    rd <- readMVar (unRH rbHandle)
    hAR <- hasActedRecently (rbTimestamp rd)
    if hAR
        then return ()
        else do
            log <- runRebalancer' (rbAppLogger rd) (rbWatchdogLogger rd)
                                    (rbRPCAuth rd) (rbMtGoxAPIHandles rd)
                                    (rbSafetyMargin rd)
            case log of
                Just (WillRebalance _) -> updateTimestamp
                _ -> return ()
  where
    updateTimestamp = do
        newTimestamp <- Just <$> getCurrentTime
        rd <- readMVar (unRH rbHandle)
        let rd' = rd { rbTimestamp = newTimestamp }
        _ <- swapMVar (unRH rbHandle) rd'
        return ()

hasActedRecently :: Maybe UTCTime -> IO Bool
hasActedRecently Nothing = return False
hasActedRecently (Just timestamp) = do
    now <- getCurrentTime
    let age = diffUTCTime now timestamp
    return $ age < timeBetweenActions

runRebalancer' :: Logger-> Maybe WatchdogLogger-> RPCAuth-> MtGoxAPIHandles-> Integer-> IO (Maybe RebalancerLog)
runRebalancer' appLogger mLogger rpcAuth mtgoxHandles safetyMargin = do
    values <- runMaybeT $ do
        tB <- MaybeT readTargetBalance
        zCB <- liftIO $ btcAmount <$> getBalanceR mLogger rpcAuth 0 True
        eCB <- liftIO $ btcAmount <$> getBalanceR mLogger rpcAuth
                                                    confsNeededForSending True
        mB <- MaybeT $ fmap piBtcBalance <$>
                                callHTTPApi mtgoxHandles getPrivateInfoR
        return (tB, zCB, eCB, mB)
    case values of
        Nothing -> let msg = RebalancerFailure "Preconditions of rebalancer not\
                                               \ fulfilled; returning early."
                   in appLogger msg >> return Nothing
        Just (tB, zCB, eCB, mB) -> do
            let (log, action) = decideRebalance tB zCB eCB mB safetyMargin
            case log of
                NothingDoTo -> return ()
                WillRebalance level ->
                    let msg = RebalancerStatus level True $
                                "Imbalance of level " ++ show level ++ "; will\
                                \ take action."
                    in appLogger msg
                UnableToRebalance level ->
                    let msg = RebalancerStatus level False $
                                "Imbalance of level " ++ show level ++ "; but\
                                \ unable to take action."
                    in appLogger msg
            case action of
                DoNothing -> return ()
                MtGoxToBitcoind v -> mtgoxToBitcoind v
                BitcoindToMtGox v -> bitcoindToMtgox v
            return $ Just log
  where
    mtgoxToBitcoind v = do
        addr <- adjustAddr <$> getNewAddressR Nothing rpcAuth
        status <- callHTTPApi' mtgoxHandles withdrawBitcoins addr v
        let msg = RebalancerAction $
                    "Moving coins from MtGox to Bitcoind; details: "
                        ++ show status ++ "."
        appLogger msg
    bitcoindToMtgox v = do
        addrM <- fmap adjustAddr <$>
                    callHTTPApi mtgoxHandles getBitcoinDepositAddressR
        case addrM of
            Nothing -> let msg = RebalancerFailure
                                    "Unable to get deposit address at MtGox."
                       in appLogger msg
            Just addr -> do
                status <- sendToAddress rpcAuth addr (BitcoinAmount v)
                let msg = RebalancerAction $
                            "Moving coins from Bitcoind to MtGox; details: "
                                ++ show status ++ "."
                appLogger msg

decideRebalance :: Integer-> Integer-> Integer-> Integer-> Integer-> (RebalancerLog, RebalancerAction)
decideRebalance targetBalance zeroConfBalance enoughConfBalance mtgoxBalance safetyMargin =
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
        let transfer' = min (enoughConfBalance - safetyMargin) transfer
        in if transfer' >= smallestCoin
            then (WillRebalance diffLevel, BitcoindToMtGox transfer')
            else (UnableToRebalance diffLevel, DoNothing)
      | transfer < 0 =
        let transfer' = min (mtgoxBalance - safetyMargin) (-1 * transfer)
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
