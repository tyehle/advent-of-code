module Y2018.D15 where

import Data.Functor (($>))
import Data.List (sort, groupBy)
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Parsec
import Text.Parsec.Char

import ParseUtil
import Interactive


type Loc = (Int, Int)
type Cave = Set Loc
data Team = Elf | Goblin deriving (Show, Eq)
data Unit = Unit Team Int Loc deriving (Show, Eq)

instance Ord Unit where
  compare (Unit _ _ (x1, y1)) (Unit _ _ (x2, y2)) = compare (y1, x1) (y2, x2)

enemy :: Team -> Team
enemy Elf = Goblin
enemy Goblin = Elf



run :: String -> IO ()
run fileName = do
  (cave, units) <- unsafeParse parseWorld fileName <$> readFile fileName
  runInteraction prettyWorld damage changeTeams (cave, units)
  where
    changeTeams (cave, units) = (cave, map (\(Unit t hp loc) -> Unit (enemy t) hp loc) units)
    damage (cave, units) = (cave, map (\(Unit t hp loc) -> Unit t (hp - 1) loc) units)


adjacent :: Cave -> Loc -> [Loc]
adjacent cave (x, y) = filter (`Set.member` cave) [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]



---------- pretty printing ----------

prettyWorld :: (Cave, [Unit]) -> String
prettyWorld (cave, units) = go [] [] 0 0 [] (sort units)
  where
    width = max (Set.findMax $ Set.map fst cave) (maximum $ map (\(Unit _ _ (x, _)) -> x) units)
    height = max (Set.findMax $ Set.map snd cave) (maximum $ map (\(Unit _ _ (_, y)) -> y) units)

    nextUnitHere :: Loc -> [Unit] -> Bool
    nextUnitHere _ [] = False
    nextUnitHere here (Unit _ _ loc: _) = here == loc

    unitGlyph :: Unit -> String
    unitGlyph (Unit Elf _ _) = "\ESC[1;32mE\ESC[0m"
    unitGlyph (Unit Goblin _ _) = "\ESC[1;31mG\ESC[0m"

    prettyUnit :: Unit -> String
    prettyUnit unit@(Unit _ hp _) = unitGlyph unit ++ "(" ++ show hp ++ ")"

    go :: [String] -> [String] -> Int -> Int -> [Unit] -> [Unit] -> String
    go rows partialRow x y unitsInRow remainingUnits
      | y > height+1 = unlines . reverse $ rows

      | x > width+1 =
        let fullRow = concat $ reverse partialRow
            newRow = fullRow ++ "          " ++ (unwords . reverse . map prettyUnit $ unitsInRow)
        in go (newRow:rows) [] 0 (y+1) [] remainingUnits

      | nextUnitHere (x, y) remainingUnits =
        let partialRow' = unitGlyph (head remainingUnits) : partialRow
            unitsInRow' = head remainingUnits : unitsInRow
            remainingUnits' = tail remainingUnits
        in go rows partialRow' (x+1) y unitsInRow' remainingUnits'

      | otherwise =
        let newChar = if Set.member (x, y) cave then " " else "\ESC[38;5;8m#\ESC[0m"
        in go rows (newChar:partialRow) (x+1) y unitsInRow remainingUnits



---------- parsing ----------

parseWorld :: Parsec String () (Cave, [Unit])
parseWorld = go (Set.empty, [])
  where
    go :: (Cave, [Unit]) -> Parsec String () (Cave, [Unit])
    go world = eof $> world
            <|> (updateWorld world <$> parseTile >>= go)
    updateWorld :: (Cave, [Unit]) -> (Maybe Loc, Maybe Unit) -> (Cave, [Unit])
    updateWorld (cave, units) (maybeLoc, maybeUnit) = (maybe cave (`Set.insert` cave) maybeLoc, maybe units (:units) maybeUnit)


parseTile :: Parsec String () (Maybe Loc, Maybe Unit)
parseTile  =  char '#' $> (Nothing, Nothing)
          <|> newline  $> (Nothing, Nothing)
          <|> do { char '.'; loc <- parserLoc; return (Just loc, Nothing) }
          <|> do { char 'G'; loc <- parserLoc; return (Just loc, Just $ Unit Goblin 200 loc) }
          <|> do { char 'E'; loc <- parserLoc; return (Just loc, Just $ Unit Elf 200 loc) }

parserLoc :: Parsec String () Loc
parserLoc = do
  sourcePos <- getPosition
  return (sourceColumn sourcePos - 2, sourceLine sourcePos - 1)
