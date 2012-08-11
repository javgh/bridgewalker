module Main where

import Test.Framework

import ScrambleCredentialsTests

tests :: [Test]
tests = scrambleCredentialsTests

main :: IO ()
main = defaultMain tests
