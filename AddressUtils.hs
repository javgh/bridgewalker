module AddressUtils
    ( adjustAddr
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

adjustAddr :: (IsBitcoinAddress a, IsBitcoinAddress b) => a -> b
adjustAddr = textToAddr . addrToText
