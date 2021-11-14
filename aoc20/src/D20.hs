module D20 where

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (isJust)

import CInt
import Parsing (split, parseGrid, printGrid)
import GHC.OldList (reverse)
import GHC.Maybe (Maybe(Nothing))
import Control.Monad (guard)
import Data.Foldable (find)


type Tile = (Integer, Map C Char)

-- Horizontal flip?, clockwise rotations
type Orientation = (Bool, Integer)


parse :: String -> [Tile]
parse = map parseTile . flip split "\n\n"
  where
    parseTile block = (read $ drop 5 $ init header, parseGrid $ unlines body)
      where (header : body) = lines block


-- | Clockwise from the upper left corner
tileEdge :: Tile -> Orientation -> Integer -> [Char]
tileEdge (_, grid) (flipped, rotations) edgeNumber =
  (if flipped then reverse else id) $ case orientedEdgeNumber of
    0 -> [grid ! (x, 0) | x <- [0..maxX]]
    1 -> [grid ! (maxX, y) | y <- [0..maxY]]
    2 -> [grid ! (x, maxY) | x <- reverse [0..maxX]]
    3 -> [grid ! (0, y) | y <- reverse [0..maxY]]
    bad -> error $ "You goofed the mod: " ++ show bad
  where
    (maxX, maxY) = maximum $ Map.keys grid
    -- flip then rotate
    orientedEdgeNumber = (edgeNumber - rotations + (if flipped && odd edgeNumber then 2 else 0)) `mod` 4


tileMatches :: [Tile] -> Map Tile [(Orientation, (Tile, Orientation))]
tileMatches tiles = Map.fromList [(t, allMatchingTiles t) | t <- tiles]
  where
    orientations :: [Orientation]
    orientations = [(flipped, side) | flipped <- [False, True], side <- [0..3]]

    allEdges :: Map (Tile, Orientation) [Char]
    allEdges = Map.fromList [((t, o), tileEdge t o 0) | t <- tiles, o <- orientations]

    tilesMatch :: (Tile, Orientation) -> (Tile, Orientation) -> Bool
    tilesMatch a (tile, (flipped, rotations)) = allEdges ! a == reverse (allEdges ! (tile, (flipped, (rotations + 2) `mod` 4)))

    matchingTiles :: (Tile, Orientation) -> Maybe (Tile, Orientation)
    matchingTiles me = case [(t, o) | t <- tiles, t /= fst me, o <- orientations, tilesMatch me (t, o)] of
      [] -> Nothing
      [match] -> Just match
      xs -> error "Found more than one matching tile"

    allMatchingTiles :: Tile -> [(Orientation, (Tile, Orientation))]
    allMatchingTiles tile = do
      o <- orientations
      case matchingTiles (tile, o) of
        Nothing -> []
        Just match -> [(o, match)]


part1 :: Map Tile [(Orientation, (Tile, Orientation))] -> Integer
part1 = product . map (fst . fst) . filter ((== 4) . length  . snd) . Map.toList


-- buildGrid :: Map Tile [(Orientation, (Tile, Orientation))] -> Map C Char
buildGrid matches = map fst $ matches ! corner
  where
    corner :: Tile
    Just corner = find ((== 4) . length . (matches !)) $ Map.keys matches

    orientation = (False, _)

    down, right :: C
    down = (0, 1)
    right = (1, 0)


-- rotateTile :: Map C Char -> Map C Char
-- rotateTile tile = Map.mapKeys (subtract minKey) rotated
--   where
--     rotated = Map.mapKeys (\k -> rotate k (0, -1) 1) tile
--     minKey = minimum $ Map.keys rotated

-- flipTile :: Map C Char -> Map C Char
-- flipTile = Map.mapKeys $ \(x, y) -> (y, x)


run :: IO ()
run = do
  input <- parse <$> readFile "input/20"
  let edgeMatches = tileMatches input
  print $ part1 edgeMatches
  -- mapM_ (putStrLn . printGrid . snd) $ take 3 input
  -- print . Map.elems . Map.map length . tileMatches $ input
  print $ buildGrid edgeMatches


example :: [Tile]
example = parse
  "Tile 2311:\n\
  \..##.#..#.\n\
  \##..#.....\n\
  \#...##..#.\n\
  \####.#...#\n\
  \##.##.###.\n\
  \##...#.###\n\
  \.#.#.#..##\n\
  \..#....#..\n\
  \###...#.#.\n\
  \..###..###\n\
  \\n\
  \Tile 1951:\n\
  \#.##...##.\n\
  \#.####...#\n\
  \.....#..##\n\
  \#...######\n\
  \.##.#....#\n\
  \.###.#####\n\
  \###.##.##.\n\
  \.###....#.\n\
  \..#.#..#.#\n\
  \#...##.#..\n"
