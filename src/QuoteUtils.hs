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

compileQuote :: BridgewalkerHandles-> BridgewalkerAccount -> AmountType -> IO (QuoteCompilation QuoteData)
compileQuote bwHandles account amountType = runAsQuoteCompilation $ do
    let typicalTxFee = bcTypicalTxFee . bhConfig $ bwHandles
        actualFee = getActualFee bwHandles
        dbConn = bhDBConnCH bwHandles
    -- some preparation
    isSmallTx <- needsSmallTxFund bwHandles amountType
    feeOverheadUSD <- if isSmallTx
        then do
            usdAmount <- simulateBTCBuy' bwHandles typicalTxFee
            return $ addFee usdAmount actualFee
        else return 0
    -- main calculation
    pqd <- case amountType of
        AmountBasedOnBTC btc -> do
            usdNeeded <- simulateBTCBuy' bwHandles btc
            let usdNeededWithFee = addFee usdNeeded actualFee
            usdForRecipient <- simulateBTCSell' bwHandles btc
            return PureQuoteData { pqdBTC = btc
                                 , pqdUSDRecipient = usdForRecipient
                                 , pqdUSDAccount = usdNeededWithFee
                                                      + feeOverheadUSD
                                 }
        AmountBasedOnUSDBeforeFees usdForRecipient -> do
            btc <- simulateUSDBuy' bwHandles usdForRecipient
            usdNeeded <- simulateBTCBuy' bwHandles btc
            let usdNeededWithFee = addFee usdNeeded actualFee
            return PureQuoteData { pqdBTC = btc
                                 , pqdUSDRecipient = usdForRecipient
                                 , pqdUSDAccount = usdNeededWithFee
                                                      + feeOverheadUSD
                                 }
        AmountBasedOnUSDAfterFees usdFromOurAccount -> do
            let usdBeforeFee =
                    subtractFee (usdFromOurAccount - feeOverheadUSD) actualFee
            btc <- simulateUSDSell' bwHandles usdBeforeFee
            usdForRecipient <- simulateBTCSell' bwHandles btc
            return PureQuoteData { pqdBTC = btc
                                 , pqdUSDRecipient = usdForRecipient
                                 , pqdUSDAccount = usdFromOurAccount
                                 }
    -- fill in some additional infos
    usdBalance <- liftIO $ getUSDBalance dbConn (bAccount account)
    return QuoteData { qdBTC = pqdBTC pqd
                     , qdUSDRecipient = pqdUSDRecipient pqd
                     , qdUSDAccount = pqdUSDAccount pqd
                     , qdSufficientBalance = pqdUSDAccount pqd <= usdBalance
                     , qdNeedsSmallTxFund = isSmallTx
                     }

needsSmallTxFund :: MonadIO m =>BridgewalkerHandles -> AmountType -> EitherT QCError m Bool
needsSmallTxFund bwHandles amountType = do
    let minimumOrderBTC = bcMtGoxMinimumOrderBTC . bhConfig $ bwHandles
    btcAmount <- case amountType of
                    AmountBasedOnBTC btc -> return btc
                    AmountBasedOnUSDBeforeFees usd ->
                        simulateUSDBuy' bwHandles usd
                    AmountBasedOnUSDAfterFees usd ->
                        let actualFee = getActualFee bwHandles
                            usdBeforeFee = subtractFee usd actualFee
                        in simulateUSDSell' bwHandles usdBeforeFee
    return $ btcAmount < minimumOrderBTC

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
