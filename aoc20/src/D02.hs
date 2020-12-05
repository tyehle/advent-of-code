module D02 where

import Text.Megaparsec (some)
import Text.Megaparsec.Char

import Parsing

parse :: String -> [(Int, Int, Char, String)]
parse = unsafeParse (linesP line)
  where
    line = do
      low <- digitsP
      char '-'
      high <- digitsP
      space1
      c <- letterChar
      string ": "
      s <- some letterChar
      return (low, high, c, s)

getInput :: IO [(Int, Int, Char, String)]
getInput = parse <$> readFile "input/02"

part1 :: [(Int, Int, Char, String)] -> Int
part1 = length . filter isValid
  where
    isValid (l, h, c, s) =
      let n = length $ filter (== c) s
      in l <= n && n <= h

part2 :: [(Int, Int, Char, String)] -> Int
part2 = length . filter isValid
  where
    isValid (x, y, c, s) =
      let isX = s !! (x-1) == c
          isY = s !! (y-1) == c
      in isX && not isY || not isX && isY

run :: IO ()
run = do
  input <- getInput
  print $ part1 input
  print $ part2 input
