module D11 where

import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map

import Parsing

type Grid = Map (Int, Int) Char

parse :: String -> Grid
parse = parseGrid

step :: Grid -> Grid
step grid = Map.mapWithKey stepCell grid
  where
    adjacent (x, y) = [(x', y') | x' <- [x-1, x, x+1], y' <- [y-1, y, y+1], (x', y') /= (x, y)]
    count pos = length $ filter occupied $ adjacent pos
    occupied pos = Map.lookup pos grid == Just '#'
    stepCell pos '.' = '.'
    stepCell pos 'L' = if count pos == 0 then '#' else 'L'
    stepCell pos '#' = if count pos >= 4 then 'L' else '#'

part1 :: Grid -> Int
part1 = Map.size . Map.filter (== '#') . go
  where
    go input = if input == next then input else go next
      where next = step input

seatPos :: Grid -> (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
seatPos grid (x, y) (x', y') = case Map.lookup (x+x', y+y') grid of
  Just '.' -> seatPos grid (x+x', y+y') (x', y')
  Nothing -> Nothing
  _ -> Just (x+x', y+y')

part2 :: Grid -> Int
part2 = Map.size . Map.filter (== '#') . go
  where
    go input = if input == next then input else go next
      where next = step2 input

directions :: [(Int, Int)]
directions = [(x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1], (x, y) /= (0, 0)]

step2 :: Grid -> Grid
step2 grid = Map.mapWithKey stepCell grid
  where
    count pos = length $ filter (== Just '#') $ map (seatPos grid pos >=> flip Map.lookup grid) directions
    stepCell pos '.' = '.'
    stepCell pos 'L' = if count pos == 0 then '#' else 'L'
    stepCell pos '#' = if count pos >= 5 then 'L' else '#'

pprint :: Grid -> String
pprint = concatMap (\((_, col), c) -> if col == 0 then ['\n', c] else [c]) . Map.toList

run :: IO ()
run = do
  input <- parse <$> readFile "input/11"
  print $ part1 input
  print $ part2 input

test = print $ part2 $ parse input
  where
    input = "L.LL.LL.LL\n\
            \LLLLLLL.LL\n\
            \L.L.L..L..\n\
            \LLLL.LL.LL\n\
            \L.LL.LL.LL\n\
            \L.LLLLL.LL\n\
            \..L.L.....\n\
            \LLLLLLLLLL\n\
            \L.LLLLLL.L\n\
            \L.LLLLL.LL"