module QuoteUtils
    ( compileQuote
    , compileSimpleQuoteBTCBuy
    , compileSimpleQuoteBTCSell
    , QuoteCompilation(..)
    ) where

import Network.MtGoxAPI

import CommonTypes
import Config
import DbUtils

data QuoteCompilation a = SuccessfulQuote a
                        | HadNotEnoughDepth
                        | DepthStoreWasUnavailable

compileSimpleQuoteBTCBuy :: BridgewalkerHandles -> Integer -> IO (QuoteCompilation Integer)
compileSimpleQuoteBTCBuy bwHandles btcAmount = do
    let mtgoxHandles = bhMtGoxHandles bwHandles
        depthStoreHandle = mtgoxDepthStoreHandle mtgoxHandles
        mtgoxFee = bhMtGoxFee bwHandles
        targetFee = bcTargetExchangeFee . bhConfig $ bwHandles
        actualFee = max mtgoxFee targetFee
    usdAmountBuyM <- simulateBTCBuy depthStoreHandle btcAmount
    return $ case usdAmountBuyM of
                DepthStoreAnswer answer ->
                    SuccessfulQuote $ addFee answer actualFee
                NotEnoughDepth -> HadNotEnoughDepth
                DepthStoreUnavailable -> DepthStoreWasUnavailable

compileSimpleQuoteBTCSell :: BridgewalkerHandles -> Integer -> IO (QuoteCompilation Integer)
compileSimpleQuoteBTCSell bwHandles btcAmount = do
    let mtgoxHandles = bhMtGoxHandles bwHandles
        depthStoreHandle = mtgoxDepthStoreHandle mtgoxHandles
        mtgoxFee = bhMtGoxFee bwHandles
        targetFee = bcTargetExchangeFee . bhConfig $ bwHandles
        actualFee = max mtgoxFee targetFee
    usdAmountSellM <- simulateBTCSell depthStoreHandle btcAmount
    return $ case usdAmountSellM of
                DepthStoreAnswer answer ->
                    SuccessfulQuote $ addFee answer (-1 * actualFee)
                NotEnoughDepth -> HadNotEnoughDepth
                DepthStoreUnavailable -> DepthStoreWasUnavailable

compileQuote :: BridgewalkerHandles-> BridgewalkerAccount -> AmountType -> IO (QuoteCompilation QuoteData)
compileQuote bwHandles account amountType = do
    let typicalTxFee = bcTypicalTxFee . bhConfig $ bwHandles
        dbConn = bhDBConnCH bwHandles
        mtgoxHandles = bhMtGoxHandles bwHandles
        depthStoreHandle = mtgoxDepthStoreHandle mtgoxHandles
    usdBalance <- getUSDBalance dbConn (bAccount account)
    pureQuote <- compilePureQuote bwHandles amountType
    feeOverheadUSDM <- simulateBTCBuy depthStoreHandle typicalTxFee
    return $ case (pureQuote, feeOverheadUSDM) of
                (SuccessfulQuote pqd, DepthStoreAnswer feeOverheadUSD) ->
                    SuccessfulQuote $
                        augmentPureQuote bwHandles pqd feeOverheadUSD usdBalance
                (HadNotEnoughDepth, _) -> HadNotEnoughDepth
                (DepthStoreWasUnavailable, _) -> DepthStoreWasUnavailable
                (_, NotEnoughDepth) -> HadNotEnoughDepth
                (_, DepthStoreUnavailable) -> DepthStoreWasUnavailable

augmentPureQuote :: BridgewalkerHandles-> PureQuoteData -> Integer -> Integer -> QuoteData
augmentPureQuote bwHandles pqd feeOverheadUSD usdBalance =
    let minimumOrderBTC = bcMtGoxMinimumOrderBTC . bhConfig $ bwHandles
        (usdAccount, needsSmallTxFund) =
            if pqdBTC pqd < minimumOrderBTC
                then (pqdUSDAccount pqd + feeOverheadUSD, True)
                else (pqdUSDAccount pqd, False)
    in QuoteData { qdBTC = pqdBTC pqd
                 , qdUSDRecipient = pqdUSDRecipient pqd
                 , qdUSDAccount = usdAccount
                 , qdSufficientBalance = usdAccount <= usdBalance
                 , qdNeedsSmallTxFund = needsSmallTxFund
                 }

compilePureQuote :: BridgewalkerHandles-> AmountType -> IO (QuoteCompilation PureQuoteData)
compilePureQuote bwHandles amountType = do
    let mtgoxHandles = bhMtGoxHandles bwHandles
        depthStoreHandle = mtgoxDepthStoreHandle mtgoxHandles
        mtgoxFee = bhMtGoxFee bwHandles
        targetFee = bcTargetExchangeFee . bhConfig $ bwHandles
        actualFee = max mtgoxFee targetFee
    case amountType of
        AmountBasedOnBTC btc -> do
            usdAmountBuyM <- simulateBTCBuy depthStoreHandle btc
            usdAmountSellM <- simulateBTCSell depthStoreHandle btc
            return $
                case (usdAmountBuyM, usdAmountSellM) of
                    (DepthStoreAnswer usdAmountBuy,
                        DepthStoreAnswer usdAmountSell) ->
                            let usdAmountBuyWithFee =
                                    addFee usdAmountBuy actualFee
                            in SuccessfulQuote
                                PureQuoteData { pqdBTC = btc
                                              , pqdUSDRecipient = usdAmountSell
                                              , pqdUSDAccount =
                                                    usdAmountBuyWithFee
                                              }
                    (NotEnoughDepth, NotEnoughDepth) -> HadNotEnoughDepth
                    (_, _) -> DepthStoreWasUnavailable
        AmountBasedOnUSDBeforeFees usd -> do
            btcAmountM <- simulateUSDBuy depthStoreHandle usd
            case btcAmountM of
               DepthStoreAnswer btcAmount -> do
                   usdAmountNeededM <- simulateBTCBuy depthStoreHandle btcAmount
                   return $ case usdAmountNeededM of
                                DepthStoreAnswer usdAmountNeeded ->
                                    let usdAmountWithFee =
                                            addFee usdAmountNeeded actualFee
                                    in SuccessfulQuote
                                        PureQuoteData { pqdBTC = btcAmount
                                                      , pqdUSDRecipient = usd
                                                      , pqdUSDAccount =
                                                         usdAmountWithFee
                                                      }
                                NotEnoughDepth -> HadNotEnoughDepth
                                DepthStoreUnavailable
                                    -> DepthStoreWasUnavailable
               NotEnoughDepth -> return HadNotEnoughDepth
               DepthStoreUnavailable -> return DepthStoreWasUnavailable
        AmountBasedOnUSDAfterFees usd -> do
            let usdBeforeFee = subtractFee usd actualFee
            btcAmountM <- simulateUSDSell depthStoreHandle usdBeforeFee
            case btcAmountM of
               DepthStoreAnswer btcAmount -> do
                   usdAmountRecipientM <-
                       simulateBTCSell depthStoreHandle btcAmount
                   return $ case usdAmountRecipientM of
                                DepthStoreAnswer usdAmountRecipient ->
                                    SuccessfulQuote
                                        PureQuoteData { pqdBTC = btcAmount
                                                      , pqdUSDRecipient =
                                                         usdAmountRecipient
                                                      , pqdUSDAccount = usd
                                                      }
                                NotEnoughDepth -> HadNotEnoughDepth
                                DepthStoreUnavailable
                                    -> DepthStoreWasUnavailable
               NotEnoughDepth -> return HadNotEnoughDepth
               DepthStoreUnavailable -> return DepthStoreWasUnavailable

addFee :: Integer -> Double -> Integer
addFee amount fee =
    let markup = round $ fromIntegral amount * (fee / 100)
    in amount + markup

subtractFee :: Integer -> Double -> Integer
subtractFee amount fee =
    round $ fromIntegral amount * (100.0 / (100.0 + fee))
