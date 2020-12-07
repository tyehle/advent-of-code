module D05 where

import Data.List (sort)
import Numeric

parse = lines

readBin :: String -> Int
readBin = fst . head . readInt 2 (`elem` "01") (\c -> if c == '0' then 0 else 1)

pos :: String -> (Int, Int)
pos input = (row, col)
  where
    row = readBin $ [if c == 'F' then '0' else '1' | c <- take 7 input]
    col = readBin $ [if c == 'L' then '0' else '1' | c <- drop 7 input]

seatId (r, c) = r*8 + c

part1 = maximum . map (seatId . pos)

part2 input = (+1) $ fst $ head $ filter isSkip $ zip ids (tail ids)
  where
    ids = sort $ map (seatId . pos) input
    isSkip (a, b) = a + 1 /= b

run :: IO ()
run = do
  input <- parse <$> readFile "input/05"
  print $ part1 input
  print $ part2 input