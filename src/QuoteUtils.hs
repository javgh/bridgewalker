module QuoteUtils
    ( compileQuote
    , compileSimpleQuoteBTCBuy
    , compileSimpleQuoteBTCSell
    , QuoteCompilation(..)
    , QCError(..)
    ) where

import Control.Error
import Control.Monad.IO.Class
import Network.MtGoxAPI
import Network.MtGoxAPI.DepthStore

import qualified Data.Text as T

import CommonTypes
import Config
import DbUtils

data QuoteCompilation a = SuccessfulQuote a
                        | QuoteCompilationError QCError

data QCError = HadNotEnoughDepth
             | DepthStoreWasUnavailable
             deriving (Show)

compileSimpleQuoteBTCBuy :: BridgewalkerHandles -> Integer -> IO (QuoteCompilation Integer)
compileSimpleQuoteBTCBuy bwHandles btcAmount = runAsQuoteCompilation $ do
    usdAmount <- simulateBTCBuy' bwHandles btcAmount
    return $ addFee usdAmount (getActualFee bwHandles)

compileSimpleQuoteBTCSell :: BridgewalkerHandles -> Integer -> IO (QuoteCompilation Integer)
compileSimpleQuoteBTCSell bwHandles btcAmount = runAsQuoteCompilation $ do
    usdAmount <- simulateBTCSell' bwHandles btcAmount
    return $ addFee usdAmount (-1 * getActualFee bwHandles)

compileQuote :: BridgewalkerHandles-> BridgewalkerAccount -> Maybe T.Text -> AmountType -> IO (QuoteCompilation QuoteData)
compileQuote bwHandles account mAddress amountType = runAsQuoteCompilation $ do
    let typicalTxFee = bcTypicalTxFee . bhConfig $ bwHandles
        actualFee = getActualFee bwHandles
        dbConn = bhDBConnCH bwHandles
    -- some preparation
    otherAccountM <- case mAddress of
                        Nothing -> return Nothing
                        Just address ->
                            liftIO $ getAccountByAddress dbConn address
    let isInternalTransfer = isJust otherAccountM
    txFeeUSD <- do
        usdAmount <- simulateBTCBuy' bwHandles typicalTxFee
        return $ addFee usdAmount actualFee
    -- main calculation
    pqdInternal <- case amountType of
        AmountBasedOnBTC btc -> do
            usdForRecipient <- simulateBTCSell' bwHandles btc
            return PureQuoteData { pqdBTC = btc
                                 , pqdUSDRecipient = usdForRecipient
                                 , pqdUSDAccount = usdForRecipient
                                 }
        AmountBasedOnUSDBeforeFees usdForRecipient -> do
            btc <- simulateUSDBuy' bwHandles usdForRecipient
            return PureQuoteData { pqdBTC = btc
                                 , pqdUSDRecipient = usdForRecipient
                                 , pqdUSDAccount = usdForRecipient
                                 }
        AmountBasedOnUSDAfterFees usdFromOurAccount -> do
            btc <- simulateUSDBuy' bwHandles usdFromOurAccount
            return PureQuoteData { pqdBTC = btc
                                 , pqdUSDRecipient = usdFromOurAccount
                                 , pqdUSDAccount = usdFromOurAccount
                                 }
    pqdExternal <- case amountType of
        AmountBasedOnBTC btc -> do
            usdNeeded <- simulateBTCBuy' bwHandles btc
            let usdNeededWithFee = addFee usdNeeded actualFee
            usdForRecipient <- simulateBTCSell' bwHandles btc
            return PureQuoteData { pqdBTC = btc
                                 , pqdUSDRecipient = usdForRecipient
                                 , pqdUSDAccount = usdNeededWithFee
                                                      + txFeeUSD
                                 }
        AmountBasedOnUSDBeforeFees usdForRecipient -> do
            btc <- simulateUSDBuy' bwHandles usdForRecipient
            usdNeeded <- simulateBTCBuy' bwHandles btc
            let usdNeededWithFee = addFee usdNeeded actualFee
            return PureQuoteData { pqdBTC = btc
                                 , pqdUSDRecipient = usdForRecipient
                                 , pqdUSDAccount = usdNeededWithFee
                                                      + txFeeUSD
                                 }
        AmountBasedOnUSDAfterFees usdFromOurAccount -> do
            let usdBeforeFee =
                    subtractFee (usdFromOurAccount - txFeeUSD) actualFee
            if usdBeforeFee > 0
                then do
                    btc <- simulateUSDSell' bwHandles usdBeforeFee
                    usdForRecipient <- simulateBTCSell' bwHandles btc
                    return PureQuoteData { pqdBTC = btc
                                         , pqdUSDRecipient = usdForRecipient
                                         , pqdUSDAccount = usdFromOurAccount
                                         }
                else
                    return PureQuoteData { pqdBTC = 0
                                         , pqdUSDRecipient = 0
                                         , pqdUSDAccount = usdFromOurAccount
                                         }
    let pqd = if isInternalTransfer
                then pqdInternal
                else pqdExternal
    -- fill in some additional infos
    usdBalance <- liftIO $ getUSDBalance dbConn (bAccount account)
    return QuoteData { qdBTC = pqdBTC pqd
                     , qdUSDRecipient = pqdUSDRecipient pqd
                     , qdUSDAccount = pqdUSDAccount pqd
                     , qdSufficientBalance = pqdUSDAccount pqd <= usdBalance
                     }

runAsQuoteCompilation :: Monad m => EitherT QCError m a -> m (QuoteCompilation a)
runAsQuoteCompilation f = do
    result <- runEitherT f
    return $ case result of
        Right quote -> SuccessfulQuote quote
        Left errMsg -> QuoteCompilationError errMsg

getActualFee :: BridgewalkerHandles -> Double
getActualFee bwHandles =
    let mtgoxFee = bhMtGoxFee bwHandles
        targetFee = bcTargetExchangeFee . bhConfig $ bwHandles
    in max mtgoxFee targetFee

addFee :: Integer -> Double -> Integer
addFee amount fee =
    let markup = round $ fromIntegral amount * (fee / 100)
    in amount + markup

subtractFee :: Integer -> Double -> Integer
subtractFee amount fee =
    round $ fromIntegral amount * (100.0 / (100.0 + fee))

simulateBTCBuy' :: MonadIO m =>BridgewalkerHandles -> Integer -> EitherT QCError m Integer
simulateBTCBuy' = depthStoreAction simulateBTCBuy

simulateBTCSell' :: MonadIO m =>BridgewalkerHandles -> Integer -> EitherT QCError m Integer
simulateBTCSell' = depthStoreAction simulateBTCSell

simulateUSDBuy' :: MonadIO m =>BridgewalkerHandles -> Integer -> EitherT QCError m Integer
simulateUSDBuy' = depthStoreAction simulateUSDBuy

simulateUSDSell' :: MonadIO m =>BridgewalkerHandles -> Integer -> EitherT QCError m Integer
simulateUSDSell' = depthStoreAction simulateUSDSell

depthStoreAction :: MonadIO m =>(DepthStoreHandle -> t -> IO DepthStoreAnswer)-> BridgewalkerHandles -> t -> EitherT QCError m Integer
depthStoreAction action bwHandles amount = do
    let depthStoreHandle = mtgoxDepthStoreHandle . bhMtGoxHandles $ bwHandles
    answerM <- liftIO $ action depthStoreHandle amount
    case answerM of
        DepthStoreAnswer answer -> return answer
        NotEnoughDepth -> left HadNotEnoughDepth
        DepthStoreUnavailable -> left DepthStoreWasUnavailable

_debugQuoting :: Show a => IO (QuoteCompilation a) -> IO ()
_debugQuoting f = do
    result <- f
    case result of
        SuccessfulQuote quote -> print quote
        QuoteCompilationError errMsg -> print errMsg
