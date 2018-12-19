module Y2018.D15 where

import Control.Concurrent.Thread.Delay
import Control.Monad (join)
import Data.Functor (($>))
import Data.List (minimumBy)
import Data.Maybe (isJust, catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Text.Parsec
import Text.Parsec.Char

import ParseUtil
import Interactive
import Search


type Loc = (Int, Int)
type Cave = Set Loc
data Team = Elf | Goblin deriving (Show, Eq)
data Unit = Unit Team Int Loc deriving (Show, Eq)

instance Ord Unit where
  compare (Unit _ _ (x1, y1)) (Unit _ _ (x2, y2)) = compare (y1, x1) (y2, x2)

enemy :: Team -> Team
enemy Elf = Goblin
enemy Goblin = Elf

getLoc :: Unit -> Loc
getLoc (Unit _ _ l) = l

getTeam :: Unit -> Team
getTeam (Unit team _ _) = team

dead :: Unit -> Bool
dead (Unit _ hp _) = hp <= 0

getHP :: Unit -> Int
getHP (Unit _ hp _) = hp

changeHP :: (Int -> Int) -> Unit -> Unit
changeHP how (Unit team hp loc) = Unit team (how hp) loc



run :: String -> IO ()
run fileName = do
  (cave, units) <- unsafeParse parseWorld fileName <$> readFile fileName
  -- putStrLn $ prettyWorld (cave, units)
  -- delay 1000000
  -- putStr "\ESC[32F\ESC[J"
  -- putStrLn $ prettyWorld (changeTeams (cave, units))
  runInteraction (prettyWorld cave . snd) (safeMoveUnit cave) (step' cave) (0, Seq.unstableSort units)
  let (rounds, hp) = crunchyStar cave units
  putStrLn $ show rounds ++ " * " ++ show hp ++ " = " ++ show (rounds * hp)
  where
    step' :: Cave -> (Int, Seq Unit) -> (Int, Seq Unit)
    step' cave (0, units) = (0, step cave units)

    safeMoveUnit :: Cave -> (Int, Seq Unit) -> (Int, Seq Unit)
    safeMoveUnit cave (i, units)
      | i >= Seq.length units = (i, units)
      | otherwise = moveUnit cave (i, units)


crunchyStar :: Cave -> Seq Unit -> (Int, Int)
crunchyStar cave initialUnits = (rounds, sum (fmap getHP finalUnits))
  where
    (rounds, finalUnits) = go (0, initialUnits)

    oneTeam :: Seq Unit -> Bool
    oneTeam Seq.Empty = True
    oneTeam (Unit team _ _ Seq.:<| rest) = all (\(Unit t _ _) -> t == team) rest

    go :: (Int, Seq Unit) -> (Int, Seq Unit)
    go (n, units)
      | oneTeam units = (n, units)
      | otherwise = go (n+1, step cave units)



step :: Cave -> Seq Unit -> Seq Unit
step cave = Seq.unstableSort . go 0 . Seq.unstableSort
  where
    go :: Int -> Seq Unit -> Seq Unit
    go i units
      | i >= Seq.length units = units
      | otherwise = let (i', units') = moveUnit cave (i, units) in go i' units'


moveUnit :: Cave -> (Int, Seq Unit) -> (Int, Seq Unit)
moveUnit cave (i, units) = case attackWho (getLoc me) of
  (Just who) -> attack who units

  Nothing -> case moveWhere of
    Nothing -> (i+1, units)
    (Just dest) -> case attackWho dest of
      Nothing -> (i+1, Seq.adjust' (moveTo dest) i units)
      (Just who) -> attack who (Seq.adjust' (moveTo dest) i units)

  where
    me :: Unit
    me = Seq.index units i

    attackWho :: Loc -> Maybe Int
    attackWho myLoc
      | null couldAttack = Nothing
      | otherwise = Just $ minimumOn (Seq.index units) juciest
      where
        couldAttack :: [Int]
        couldAttack = Seq.findIndicesL (\(Unit team _ loc) -> getTeam me /= team && (loc `elem` adjacent cave myLoc)) units
        lowestHP :: Int
        lowestHP = minimum . map (getHP . Seq.index units) $ couldAttack
        juciest :: [Int]
        juciest = filter ((== lowestHP) . getHP . Seq.index units) couldAttack
        minimumOn :: (Foldable t, Ord b) => (a -> b) -> t a -> a
        minimumOn f = minimumBy (\x y -> f x `compare` f y)

    attack :: Int -> Seq Unit -> (Int, Seq Unit)
    attack who units'
      | dead hurtUnit = (if who < i then i else i+1, Seq.deleteAt who units')
      | otherwise = (i+1, Seq.update who hurtUnit units')
      where
        hurtUnit = changeHP (subtract 3) (Seq.index units' who)


    moveWhere :: Maybe Loc
    moveWhere
      | null possiblePaths = Nothing
      | otherwise = Just (x, y)
      where
        possibleStarts = adjacentOpen cave units (getLoc me)
        possiblePaths = catMaybes [bestPath cave units start (enemy (getTeam me)) | start <- possibleStarts]
        sortableStarts = map (\path@((x,y):_) -> (length path, (y, x))) possiblePaths
        (y, x) = snd . minimum $ sortableStarts

    moveTo :: Loc -> Unit -> Unit
    moveTo loc (Unit team hp _) = Unit team hp loc


bestPath :: Cave -> Seq Unit -> Loc -> Team -> Maybe [Loc]
bestPath cave units from to
  | targets == Seq.Empty = Nothing
  | otherwise = aStar (adjacentOpen cave units) priority done from
  where
    done :: Loc -> Bool
    done loc = isJust $ Seq.findIndexL (\(Unit team _ l) -> (team == to) && (l `elem` adjacent cave loc)) units

    priority :: Loc -> (Int, (Int, Int))
    priority l@(x, y) = (distanceToTarget l, (y, x))

    targets :: Seq Loc
    targets = join . fmap (Seq.fromList . adjacentOpen cave units . getLoc) . Seq.filter ((== to) . getTeam) $ units

    distanceToTarget :: Loc -> Int
    distanceToTarget loc = minimum . fmap (l1 loc) $ targets

    l1 :: Loc -> Loc -> Int
    l1 (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)


adjacent :: Cave -> Loc -> [Loc]
adjacent cave (x, y) = filter (`Set.member` cave) [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]


adjacentOpen :: Foldable f => Cave -> f Unit -> Loc -> [Loc]
adjacentOpen cave units loc = filter unoccupied $ adjacent cave loc
  where
    unoccupied :: Loc -> Bool
    unoccupied l = all ((/= l) . getLoc) units



---------- pretty printing ----------

prettyWorld :: Cave -> Seq Unit -> String
prettyWorld cave units = go [] [] 0 0 [] (Seq.sort units)
  where
    width = max (Set.findMax $ Set.map fst cave) (maximum $ fmap (\(Unit _ _ (x, _)) -> x) units)
    height = max (Set.findMax $ Set.map snd cave) (maximum $ fmap (\(Unit _ _ (_, y)) -> y) units)

    nextUnitHere :: Loc -> Seq Unit -> Bool
    nextUnitHere _ Seq.Empty = False
    nextUnitHere here (Unit _ _ loc Seq.:<| _) = here == loc

    unitGlyph :: Unit -> String
    unitGlyph (Unit Elf _ _) = "\ESC[1;32mE\ESC[0m"
    unitGlyph (Unit Goblin _ _) = "\ESC[1;31mG\ESC[0m"

    prettyUnit :: Unit -> String
    prettyUnit unit@(Unit _ hp _) = unitGlyph unit ++ "(" ++ show hp ++ ")"

    go :: [String] -> [String] -> Int -> Int -> [Unit] -> Seq Unit -> String
    go rows partialRow x y unitsInRow remainingUnits
      | y > height+1 = init . unlines . reverse $ rows

      | x > width+1 =
        let fullRow = concat $ reverse partialRow
            newRow = fullRow ++ "          " ++ (unwords . reverse . map prettyUnit $ unitsInRow)
        in go (newRow:rows) [] 0 (y+1) [] remainingUnits

      | nextUnitHere (x, y) remainingUnits =
        let unitHere Seq.:<| remainingUnits' = remainingUnits
            partialRow' = unitGlyph unitHere : partialRow
            unitsInRow' = unitHere : unitsInRow
        in go rows partialRow' (x+1) y unitsInRow' remainingUnits'

      | otherwise =
        let newChar = if Set.member (x, y) cave then " " else "\ESC[38;5;8m#\ESC[0m"
        in go rows (newChar:partialRow) (x+1) y unitsInRow remainingUnits



---------- parsing ----------

parseWorld :: Parsec String () (Cave, Seq Unit)
parseWorld = go (Set.empty, Seq.empty)
  where
    go :: (Cave, Seq Unit) -> Parsec String () (Cave, Seq Unit)
    go world = eof $> world
            <|> (updateWorld world <$> parseTile >>= go)
    updateWorld :: (Cave, Seq Unit) -> (Maybe Loc, Maybe Unit) -> (Cave, Seq Unit)
    updateWorld (cave, units) (maybeLoc, maybeUnit) = (maybe cave (`Set.insert` cave) maybeLoc, maybe units (Seq.:<| units) maybeUnit)


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
