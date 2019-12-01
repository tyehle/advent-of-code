module Y2018.D18 where


import Data.Functor (($>))
import Data.Foldable (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Text.Parsec
import Text.Parsec.Char


import Interactive
import ParseUtil


type Loc = (Int, Int)
data Tile = Trees | Open | Lumberyard deriving (Ord, Eq, Show)
type World = Map Loc Tile


run :: String -> IO ()
run fileName = do
  world <- unsafeParse parseInput fileName <$> readFile fileName
  print . resourceValue . stateNumber 10 $ world
  print . resourceValue . stateNumber 1000000000 $ world
  runInteraction prettyWorld id step world


stateNumber :: Int -> World -> World
stateNumber = go Map.empty
  where
    go :: Map World Int -> Int -> World -> World
    go _ 0 world = world
    go worlds n world = case Map.lookup world worlds of
      Nothing -> go (Map.insert world n worlds) (n-1) (step world)
      Just lastN -> let index = lastN - mod lastN (lastN - n)
                        found = find ((== index) . snd) $ Map.toList worlds
                      in maybe (error "Bad memory") fst found


resourceValue :: World -> Int
resourceValue world = total Trees * total Lumberyard
  where
    total tile = countTiles tile . map snd . Map.toList $ world


step :: World -> World
step world = Map.mapWithKey newTile world
  where
    adjacentTiles :: Loc -> [Tile]
    adjacentTiles = mapMaybe (`Map.lookup` world) . adjacent

    newTile :: Loc -> Tile -> Tile
    newTile loc tile = stepTile tile $ adjacentTiles loc


stepTile :: Tile -> [Tile] -> Tile
stepTile Open tiles
  | countTiles Trees tiles >= 3 = Trees
  | otherwise = Open
stepTile Trees tiles
  | countTiles Lumberyard tiles >= 3 = Lumberyard
  | otherwise = Trees
stepTile Lumberyard tiles
  | countTiles Lumberyard tiles >= 1 && countTiles Trees tiles >= 1 = Lumberyard
  | otherwise = Open


countTiles :: Tile -> [Tile] -> Int
countTiles tile = length . filter (== tile)


adjacent :: Loc -> [Loc]
adjacent (x, y) = [ (x-1, y-1), (x-1, y), (x-1, y+1)
                  , (x, y-1), (x, y+1)
                  , (x+1, y-1), (x+1, y), (x+1, y+1)
                  ]



---------- pretty printing ----------

prettyWorld :: World -> String
prettyWorld world = unlines $ go [] [] 0 0
  where
    bottom = maximum . map snd . Map.keys $ world

    tileGlyph :: Tile -> String
    tileGlyph Open       = "\ESC[33m.\ESC[0m"
    tileGlyph Trees      = "\ESC[32m|\ESC[0m"
    tileGlyph Lumberyard = "\ESC[38;5;247m#\ESC[0m"

    go :: [String] -> [String] -> Int -> Int -> [String]
    go rows row x y
      | y > bottom = reverse rows
      | not $ Map.member (x, y) world = go (concat (reverse row) : rows) [] 0 (y+1)
      | otherwise = go rows (tileGlyph (world Map.! (x, y)) : row) (x+1) y


---------- parsing ----------

parseInput :: Parsec String () World
parseInput = go Map.empty
  where
    go :: World -> Parsec String () World
    go world = eof $> world
           <|> many1 space *> go world
           <|> do
             tile <- parseTile
             loc <- parserLoc
             go $ Map.insert loc tile world

parseTile :: Parsec String a Tile
parseTile = char '.' $> Open
        <|> char '|' $> Trees
        <|> char '#' $> Lumberyard

parserLoc :: Parsec String () Loc
parserLoc = do
  sourcePos <- getPosition
  return (sourceColumn sourcePos - 2, sourceLine sourcePos - 1)
