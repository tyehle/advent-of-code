module D05 where

import Crypto.Hash (hashWith)
import Crypto.Hash.Algorithms (MD5(..))
import Data.ByteString.Char8 (pack)
import Data.Maybe (fromJust)
import Data.Foldable (find)
import Data.Char (intToDigit)

run :: IO ()
run = putStr "LR Code: " >> putStrLn lrResult >> putStr "Random Code: " >> putStrLn randResult
  where
    base = "wtnhxymk"
    hashes = filter fiveZeros . map (hash . (base ++) . show) $ [0::Int ..]
    lrResult = take 8 . map (!! 5) $ hashes
    randResult = map findDigit [0..7]
    findDigit d = (!! 6) . fromJust . find ((== intToDigit d) . (!! 5)) $ hashes

hash :: String -> String
hash = show . hashWith MD5 . pack

fiveZeros :: String -> Bool
fiveZeros = all (== '0') . take 5
