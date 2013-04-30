module QuoteUtilsTests
    ( main
    ) where

import Control.Concurrent

import Bridgewalker
import CommonTypes
import Config
import FormatUtils
import QuoteUtils

main :: IO ()
main = do
    bwHandles <- initBridgewalker
    threadDelay $ 5 * 1000 * 1000
    testQuote bwHandles $ AmountBasedOnBTC 1000000
    testQuote bwHandles $ AmountBasedOnUSDBeforeFees 10000000
    testQuote bwHandles $ AmountBasedOnUSDAfterFees 10000000

    testQuote bwHandles $ AmountBasedOnBTC 100000
    testQuote bwHandles $ AmountBasedOnUSDBeforeFees 100000
    testQuote bwHandles $ AmountBasedOnUSDAfterFees 100000

testQuote :: BridgewalkerHandles -> AmountType -> IO ()
testQuote bwHandles amount = do
    let account = BridgewalkerAccount 2
    qc <- compileQuote bwHandles account amount
    putStrLn $ "Quote for: " ++ show amount
    case qc of
        SuccessfulQuote quote -> do
            putStrLn $ "BTC: " ++ formatBTCAmount (qdBTC quote)
            putStrLn $ "USD (Recipient): "
                        ++ formatUSDAmount (qdUSDRecipient quote)
            putStrLn $ "USD (Account): " ++ formatUSDAmount (qdUSDAccount quote)
            putStrLn $ "Sufficient balance: " ++ show (qdSufficientBalance quote)
            putStrLn $ "Needs small tx fund: " ++ show (qdNeedsSmallTxFund quote)
        QuoteCompilationError errMsg -> print errMsg
    putStrLn ""
