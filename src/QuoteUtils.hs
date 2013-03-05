module QuoteUtils
    ( compileQuote
    , QuoteCompilation(..)
    ) where

import Network.MtGoxAPI

import CommonTypes
import Config
import DbUtils

data QuoteCompilation = SuccessfulQuote QuoteData
                      | HadNotEnoughDepth
                      | DepthStoreWasUnavailable

compileQuote :: BridgewalkerHandles-> BridgewalkerAccount -> AmountType -> IO QuoteCompilation
compileQuote bwHandles account amountType = do
    let mtgoxHandles = bhMtGoxHandles bwHandles
        depthStoreHandle = mtgoxDepthStoreHandle mtgoxHandles
        dbConn = bhDBConnCH bwHandles
        mtgoxFee = bhMtGoxFee bwHandles
        targetFee = bcTargetExchangeFee . bhConfig $ bwHandles
        actualFee = max mtgoxFee targetFee
    usdBalance <- getUSDBalance dbConn (bAccount account)
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
                                QuoteData { qdBTC = btc
                                          , qdUSDRecipient = usdAmountSell
                                          , qdUSDAccount = usdAmountBuyWithFee
                                          , qdSufficientBalance =
                                             usdAmountBuyWithFee
                                                 <= usdBalance
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
                                        QuoteData { qdBTC = btcAmount
                                                  , qdUSDRecipient = usd
                                                  , qdUSDAccount =
                                                     usdAmountWithFee
                                                  , qdSufficientBalance =
                                                     usdAmountWithFee
                                                         <= usdBalance
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
                                        QuoteData { qdBTC = btcAmount
                                                  , qdUSDRecipient =
                                                     usdAmountRecipient
                                                  , qdUSDAccount = usd
                                                  , qdSufficientBalance =
                                                     usd <= usdBalance
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
