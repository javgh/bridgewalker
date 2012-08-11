module RebalancerTests
    ( rebalancerTests
    ) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.QuickCheck
import Test.HUnit hiding (Test)

import Rebalancer

smallestCoin :: Integer
smallestCoin = 1000000

propSensibleRebalancer targetBalance zeroConfBalance
                       enoughConfBalance mtgoxBalance =
    let (log, action) = decideRebalance targetBalance zeroConfBalance
                                            enoughConfBalance mtgoxBalance
    in case action of
        DoNothing -> True
        MtGoxToBitcoind v -> v >= smallestCoin
        BitcoindToMtGox v -> v >= smallestCoin

performRebalancerTest t zb eb mb expectedLog expectedAction = do
    let (log, action) = decideRebalance (t * smallestCoin) (zb * smallestCoin)
                                        (eb * smallestCoin) (mb * smallestCoin)
        adjustedAction =
            case action of
                DoNothing -> DoNothing
                MtGoxToBitcoind v -> MtGoxToBitcoind $ v `div` smallestCoin
                BitcoindToMtGox v -> BitcoindToMtGox $ v `div` smallestCoin
    assertEqual "balancer log is not correct" expectedLog log
    assertEqual "balancer action is not correct" expectedAction adjustedAction

test1 :: Test
test1 = testCase "mtgox to bitcoind" $
    performRebalancerTest 40 25 25 100 (WillRebalance 1) (MtGoxToBitcoind 15)

test2 :: Test
test2 = testCase "mtgox to bitcoind, capped" $
    performRebalancerTest 40 25 25 10 (WillRebalance 1) (MtGoxToBitcoind 10)

test3 :: Test
test3 = testCase "mtgox to bitcoind, not possible" $
    performRebalancerTest 40 25 25 0 (UnableToRebalance 1) DoNothing

test4 :: Test
test4 = testCase "mtgox to bitcoind, big diff" $
    performRebalancerTest 40 5 40 100 (WillRebalance 3) (MtGoxToBitcoind 35)

test5 :: Test
test5 = testCase "already balanced" $
    performRebalancerTest 40 38 10 10 NothingDoTo DoNothing

test6 :: Test
test6 = testCase "bitcoind to mtgox" $
    performRebalancerTest 40 52 40 100 (WillRebalance 1) (BitcoindToMtGox 12)

test7 :: Test
test7 = testCase "bitcoind to mtgox, capped" $
    performRebalancerTest 40 52 5 100 (WillRebalance 1) (BitcoindToMtGox 5)

test8 :: Test
test8 = testCase "bitcoind to mtgox, not possible" $
    performRebalancerTest 40 52 0 100 (UnableToRebalance 1) DoNothing

test9 :: Test
test9 = testCase "bitcoind to mtgox, big diff" $
    performRebalancerTest 40 62 0 100 (UnableToRebalance 2) DoNothing

rebalancerTests :: [Test]
rebalancerTests =
    [ testProperty "rebalancer acts sensible" propSensibleRebalancer ]
    ++ [ test1, test2, test3, test4, test5, test6, test7, test8, test9 ]
