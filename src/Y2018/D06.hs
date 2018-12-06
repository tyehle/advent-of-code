module Y2018.D06 where

-- import Data.Char
import Data.List.Split
import Data.List
import Data.Maybe
import qualified Data.Map as Map
-- import qualified Data.Set as Set
import Data.Foldable (maximumBy)

type Point = (Int, Int)

run :: String -> IO ()
run fileName = do
  -- parse the input into a record
  points <- map parse . lines <$> readFile fileName
  let (minX, maxX) = getBoundaries (map fst points) 10
      (minY, maxY) = getBoundaries (map snd points) 10
  let grid = [(x, y) | x <- [minX..maxX], y <- [minY..maxY]]
  let closestGrid :: [(Point, Maybe Point)]
      closestGrid = map (\x -> (x, getClosest points x)) grid

  -- get original points where  closest grid points touch the edge
  let edgeClosestGrid :: [Point]
      edgeClosestGrid = catMaybes . map snd . filter (onEdge minX maxX minY maxY . fst) $ closestGrid

  -- Filter original points to remove those that touched the edge
  let filteredClosestGrid = filter (filterFunc edgeClosestGrid) closestGrid
  let areas = frequencies . catMaybes . map snd $ filteredClosestGrid

  print $ sortBy (\(_, a) (_, b) -> a `compare` b) $ Map.toList areas

  let gridDistances = map (getDistanceSum points) grid
  print . length . filter (< 10000) $ gridDistances

  -- Calculate all Areas
  print "hi"
  where
      filterFunc :: [Point] -> (Point, Maybe Point) -> Bool
      filterFunc _ (_, Nothing) = False
      filterFunc edge (_, Just closest) = closest `notElem` edge

frequencies :: Ord a => [a] -> Map.Map a Int
frequencies keys = go keys Map.empty
    where
        go :: Ord a => [a] -> Map.Map a Int -> Map.Map a Int
        go [] freqs = freqs
        go (x:xs) freqs = go xs $ Map.alter update x freqs
        update Nothing = Just 1
        update (Just x) = Just (x + 1)


onEdge :: Int -> Int -> Int -> Int -> Point -> Bool
onEdge minX maxX minY maxY (x, y)
    | (x == minX) || (x == maxX) = True
    | (y == minY) || (y == maxY) = True
    | otherwise = False

distance :: Point -> Point -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) +  abs (y1 - y2)

getBoundaries :: [Int] -> Int -> Point
getBoundaries xs buffer = (minimum xs - buffer, maximum xs + buffer)

getClosest :: [Point] -> Point -> Maybe Point
getClosest [] _ = Nothing
getClosest (p:ps) target = go ps (distance target p) (Just p)
    where
        go :: [Point] -> Int -> Maybe Point -> Maybe Point
        -- remainingPoints, best distance, best point, Closest point
        go [] _ best = best
        go (x:xs) bestDistance best
            | currentDistance < bestDistance = go xs currentDistance (Just x)
            | currentDistance == bestDistance = go xs currentDistance Nothing
            | otherwise = go xs bestDistance best
            where currentDistance = distance x target


-- getClosest points x = minimumBy (\a b -> distance a x `compare` distance b x) points

parse :: String -> Point
parse str = (read x', read y')
    where [x', y'] = splitOn ", " str


-- Part 2
getDistanceSum :: [Point] -> Point -> Int
getDistanceSum points target = sum . map (distance target) $ points
