module ScrambleCredentials
    ( scrambleText
    , unScrambleText
    , hardcodedKeyA
    , hardcodedKeyB
    ) where

scrambleText :: String -> (String, [Integer])
scrambleText plaintext =
    let pairs = map go plaintext
        ciphertext = map fst pairs
        key = concatMap snd pairs
    in (ciphertext, key)
  where
    go c
      | '0' <= c && c <= '9' = ('0', [read [c] * 42])
      | otherwise = (c, [])

unScrambleText :: String -> [Integer] -> String
unScrambleText [] _ = []
unScrambleText cs [] = cs
unScrambleText (c:cs) (k:ks)
    | c == '0' = asChar k : unScrambleText cs ks
    | otherwise = c : unScrambleText cs (k : ks)
  where
    asChar k = head . show . round $ fromIntegral k / 42

hardcodedKeyA :: [Integer]
hardcodedKeyA = [210,378,84,336,252,336,210,168,0,336,336,42,336,84,210,126]

hardcodedKeyB :: [Integer]
hardcodedKeyB = [0,168,210,126,126,210,126,0,378,42,168,168,84,168,42]
