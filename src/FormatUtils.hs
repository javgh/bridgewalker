module FormatUtils
    ( formatBTCAmount
    , formatUSDAmount
    ) where

import Text.Printf
import Text.Regex

formatBTCAmount :: Integer -> String
formatBTCAmount a =
    let a' = fromIntegral a / 10 ^ (8 :: Integer) :: Double
        str = printf "%.8f" a'
    in subRegex (mkRegex "\\.?0+$") str ""

formatUSDAmount :: Integer -> String
formatUSDAmount a =
    let a' = fromIntegral a / 10 ^ (5 :: Integer) :: Double
        str = printf "%.5f" a'
    in subRegex (mkRegex "\\.?0+$") str ""
