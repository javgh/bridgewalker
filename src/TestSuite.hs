module Main where

import Test.Framework

import RebalancerTests
import ScrambleCredentialsTests

tests :: [Test]
tests = scrambleCredentialsTests ++ rebalancerTests

main :: IO ()
main = defaultMain tests
