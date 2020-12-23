{-# LANGUAGE TupleSections #-}
module D17 where

import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map

import Parsing (parseGrid)
import QInt

type P = (Integer, Integer, Integer)

parse :: String -> Map H Char
parse = Map.mapKeys (\(x, y) -> (x, y, 0, 0)) . parseGrid

adjacent3 :: H -> [H]
adjacent3 (x, y, z, w) = [(x+x', y+y', z+z', w) | x' <- diff, y' <- diff, z' <- diff, (x', y', z') /= (0, 0, 0)]
  where diff = [-1, 0, 1]

step :: (H -> [H]) -> Map H Char -> Map H Char
step adj world = Map.mapWithKey updateCell (growWorld adj world)
  where
    updateCell :: H -> Char -> Char
    updateCell loc value
      | value == '#' = if activeNeighbors loc `elem` [2, 3] then '#' else '.'
      | value == '.' = if activeNeighbors loc == 3 then '#' else '.'
      | otherwise = error $ "Bad value: " ++ show value
      where
        activeNeighbors = length . filter ((== Just '#') . flip Map.lookup world) . adj

growWorld :: (H -> [H]) -> Map H Char -> Map H Char
growWorld adj world = foldl' ensureAdjacent world activePoints
  where
    activePoints = Map.keys $ Map.filter (== '#') world
    ensureAdjacent world loc = Map.union world $ Map.fromList $ map (,'.') $ adj loc

part1 = Map.size . Map.filter (== '#') . (!! 6) . iterate (step adjacent3)

part2 = Map.size . Map.filter (== '#') . (!! 6) . iterate (step neighbors)

run :: IO ()
run = do
  input <- parse <$> readFile "input/17"
  print $ part1 input
  print $ part2 input
