module AddressUtils
    ( adjustAddr
    , adjustAmount
    , IsBitcoinAddress
    ) where

import qualified Data.Text as T

import qualified Network.BitcoinRPC as RPC
import qualified Network.MtGoxAPI as MtGox

class IsBitcoinAddress a where
    addrToText :: a -> T.Text
    textToAddr :: T.Text -> a

instance IsBitcoinAddress RPC.BitcoinAddress
  where
    addrToText = RPC.btcAddress
    textToAddr = RPC.BitcoinAddress

instance IsBitcoinAddress MtGox.BitcoinAddress
  where
    addrToText = MtGox.baAddress
    textToAddr = MtGox.BitcoinAddress

instance IsBitcoinAddress MtGox.BitcoinDepositAddress
  where
    addrToText = addrToText . MtGox.bdaAddr
    textToAddr = MtGox.BitcoinDepositAddress . textToAddr

instance IsBitcoinAddress T.Text
  where
    addrToText = id
    textToAddr = id

class IsBitcoinAmount a where
    amountToInteger :: a -> Integer
    integerToAmount :: Integer -> a

instance IsBitcoinAmount Integer
  where
    amountToInteger = id
    integerToAmount = id

instance IsBitcoinAmount RPC.BitcoinAmount
  where
    amountToInteger = RPC.btcAmount
    integerToAmount = RPC.BitcoinAmount

adjustAddr :: (IsBitcoinAddress a, IsBitcoinAddress b) => a -> b
adjustAddr = textToAddr . addrToText

adjustAmount :: (IsBitcoinAmount a, IsBitcoinAmount b) => a -> b
adjustAmount = integerToAmount . amountToInteger
