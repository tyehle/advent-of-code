module D03 where

import Data.Map (Map)
import qualified Data.Map as Map

import Parsing

parse :: String -> Map (Int, Int) Char
parse = parseGrid

getInput :: IO (Map (Int, Int) Char)
getInput = parse <$> readFile "input/03"

part1 :: Map (Int, Int) Char -> Int
part1 = hits (3, 1)

hits :: (Int, Int) -> Map (Int, Int) Char -> Int
hits (xStep, yStep) world = go (0, 0)
  where
    width = 1 + maximum (map fst $ Map.keys world)
    step (x, y) = ((x + xStep) `mod` width, y + yStep)
    go pos = case Map.lookup pos world of
      Nothing -> 0
      Just '#' -> 1 + go (step pos)
      Just '.' -> go (step pos)

part2 world = product $ map (flip hits world) [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

run :: IO ()
run = do
  input <- getInput
  print $ part1 input
  print $ part2 input

test = part1 $ parse testInput
  where
    testInput = "..##.......\n\
                \#...#...#..\n\
                \.#....#..#.\n\
                \..#.#...#.#\n\
                \.#...##..#.\n\
                \..#.##.....\n\
                \.#.#.#....#\n\
                \.#........#\n\
                \#.##...#...\n\
                \#...##....#\n\
                \.#..#...#.#"