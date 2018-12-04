module Y2018.D03 where

import Data.List.Split
import Data.List

data Record = Record {left :: Int, top :: Int, width :: Int, height :: Int} deriving Show

run :: IO ()
run = do
  fileName <- getLine

  -- parse the input into a record
  records <- map parse . lines <$> readFile fileName

  -- build association List gridpoints to is populated
  let vals = concatMap associationList records

  -- merge all maps with count
  print $ merge vals

parse :: String -> Record
parse xs = Record (read left') (read $ init top') width' height'
  where
      [_, _, position, size] = words xs
      [left', top'] = splitOn "," position
      [width', height'] = map read $ splitOn "x" size

associationList :: Record -> [(Int, Int)]
-- [x y | x <- [top..top + width], y <- [left..left + height]]
associationList (Record left' top' width' height') = do
    x <- [top'..top' + height' - 1]
    y <- [left'..left' + width' - 1]
    return (x, y)

-- merge all maps with count
merge :: [(Int, Int)] -> Int
merge xs = length filtered
    where
        grouped = groupBy (\(x1, y1) (x2, y2) -> (x1 == x2) && (y1 == y2)) $ sort xs
        lengths = map length grouped
        filtered = filter (> 1) lengths
