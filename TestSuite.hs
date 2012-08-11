module Main where

import Test.Framework

import ScrambleCredentialsTests
import RebalancerTests

tests :: [Test]
tests = scrambleCredentialsTests ++ rebalancerTests

main :: IO ()
main = defaultMain tests
