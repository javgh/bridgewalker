module ScrambleCredentials
    ( scrambleText
    , unScrambleText
    , hardcodedKeyA
    , hardcodedKeyB
    , hardcodedKeyC
    , hardcodedKeyD
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
    asChar k' = head . show .
                    (round :: Double -> Integer) $ fromIntegral k' / 42

hardcodedKeyA :: [Integer]
hardcodedKeyA = [378,210,126,336,378,336,378,168,84,378,0,210,168,294,0,252,84,294,168,210,126]

hardcodedKeyB :: [Integer]
hardcodedKeyB = [210,126,210,84,126,168,126,168,252,0,336,42,336]

hardcodedKeyC :: [Integer]
hardcodedKeyC = [168,84,252,210,168,168,336,42,84,84,336,378,42,378,378,336,378]

hardcodedKeyD :: [Integer]
hardcodedKeyD = [210,294,42,378,294,168,84,0,126,126,210,42,378,126,378,168,42,168]
