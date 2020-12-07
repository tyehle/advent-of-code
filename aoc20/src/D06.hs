module D06 where

import Data.List

parse :: String -> [[String]]
parse = go [] [] . lines
  where
    go g gs [] = g : gs
    go g gs ("":other) = go [] (g : gs) other
    go g gs (line:other) = go (line : g) gs other

part1 = sum . map (length . nub . sort . concat)

part2 = sum . map count
  where
    count g = let n = length g in length $ filter ((== n) . length) $ group $ sort $ concat g

run :: IO ()
run = do
  input <- parse <$> readFile "input/06"
  print $ part1 input
  print $ part2 input