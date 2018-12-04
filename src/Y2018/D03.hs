module Y2018.D03 where

import Data.List.Split
import Data.List

data Record = Record {num :: Int, left :: Int, top :: Int, width :: Int, height :: Int} deriving (Show, Eq)

run :: IO ()
run = do
  fileName <- getLine
  -- parse the input into a record
  records <- map parse . lines <$> readFile fileName
  print $ roughStar records
  print $ smoothStar records

parse :: String -> Record
parse xs = Record (read $ tail hash) (read left') (read $ init top') width' height'
  where
      [hash, _, position, size] = words xs
      [left', top'] = splitOn "," position
      [width', height'] = map read $ splitOn "x" size

associationList :: Record -> [(Int, Int)]
-- [x y | x <- [top..top + width], y <- [left..left + height]]
associationList (Record _ left' top' width' height') = do
    x <- [top'..top' + height' - 1]
    y <- [left'..left' + width' - 1]
    return (x, y)

-- merge all maps with count
merge :: [(Int, Int)] -> [Int]
merge xs = map length grouped
  where
    grouped = groupBy (\(x1, y1) (x2, y2) -> (x1 == x2) && (y1 == y2)) $ sort xs


roughStar :: [Record] -> Int
roughStar records = length $ filter (> 1) $ merge points
  where
    points = concatMap associationList records


overlaps :: Record -> Record -> Bool
overlaps a b = any (> 1) $ merge (as ++ bs)
  where
    as = associationList a
    bs = associationList b


smoothStar :: [Record] -> Maybe Record
smoothStar records = findGood records
  where
    findGood [] = Nothing
    findGood (r:rs)
      | all (not . overlaps r) (filter (/= r) records) = Just r
      | otherwise = findGood rs
