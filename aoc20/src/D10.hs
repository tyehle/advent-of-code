module D10 where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

import Data.List

parse :: String -> [Int]
parse = fmap read . lines

part1 input = do { one <- lookup 1 counts; three <- lookup 3 counts; return (one * (three + 1)) }
  where
    sorted = sort input
    counts = map (\diffs -> (head diffs, length diffs)) $ group $ sort $ zipWith (-) sorted (0:sorted)

part2 input = evalState (go sorted) Map.empty
  where
    sorted = 0 : sort ((maximum input + 3) : input)
    go :: [Int] -> State (Map [Int] Int) Int
    go [_] = return 1
    go items@(n:rest) = do
      res <- gets $ Map.lookup items
      case res of
        Just result -> return result
        Nothing -> do
          let next = filter (\xs -> not (null xs) && (head xs <= n + 3)) $ tails rest
          result <- sum <$> mapM go next
          modify $ Map.insert items result
          return result

run :: IO ()
run = do
  input <- parse <$> readFile "input/10"
  print $ part1 input
  print $ part2 input