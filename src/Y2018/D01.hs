module Y2018.D01 where

import qualified Data.Set as Set

run :: IO ()
run = do
  fileName <- getLine
  file <- readFile fileName
  let ints = parseInputList file
  print $ collectJuicyStar ints
  print $ collectPrickleyStar ints

parseInputList :: String -> [Int]
parseInputList = map (read . dropPlus) . lines
  where
    dropPlus xs = if '+' `elem` xs then tail xs else xs

collectJuicyStar :: [Int] -> Int
collectJuicyStar = sum

collectPrickleyStar :: [Int] -> Int
collectPrickleyStar = recurseList 0 Set.empty . cycle
  where recurseList :: Int -> Set.Set Int -> [Int] -> Int
        recurseList currVal set xs
            | currVal `Set.member` set = currVal
            | otherwise = recurseList (currVal + head xs) (Set.insert currVal set) (tail xs)
