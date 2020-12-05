module D01 where

parse :: String -> [Int]
parse = fmap read . lines

getInput :: IO [Int]
getInput = parse <$> readFile "input/01"

part1 :: [Int] -> Int
part1 input = head results
  where
    results = do
      a <- input
      b <- input
      if a + b == 2020 then [a * b] else []

part2 :: [Int] -> Int
part2 input = head results
  where
    results = do
      a <- input
      b <- input
      c <- input
      if a + b + c == 2020 then [a * b * c] else []

run :: IO ()
run = do
  input <- getInput
  print $ part1 input
  print $ part2 input