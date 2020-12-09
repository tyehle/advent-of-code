module D09 where

import Control.Monad

parse :: String -> [Int]
parse = map read . lines

part1 :: [Int] -> Maybe Int
part1 input = go (take 25 input) (drop 25 input)
  where
    go _ [] = Nothing
    go vals (x:xs) = case [True | a <- vals, b <- vals, a /= b, a + b == x] of
      True : _ -> go (x:init vals) xs
      [] -> Just x

part2 :: [Int] -> Int
part2 input = minimum range + maximum range
  where
    (Just n) = part1 input
    range = head $ do
      start <- [0..length input]
      end <- [0..length input]
      guard $ start+1 < end
      let part = drop start $ take end input
      guard $ sum part == n
      return part

run :: IO ()
run = do
  input <- parse <$> readFile "input/09"
  print $ part1 input
  print $ part2 input