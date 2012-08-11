module ScrambleCredentialsTests
    ( scrambleCredentialsTests
    ) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import ScrambleCredentials

propReversible :: String -> Bool
propReversible plaintext =
    let (ciphertext, key) = scrambleText plaintext
    in plaintext == unScrambleText ciphertext key

scrambleCredentialsTests :: [Test]
scrambleCredentialsTests =
    [ testProperty "scramble is reversible" propReversible ]
