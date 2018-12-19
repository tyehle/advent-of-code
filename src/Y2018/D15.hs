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


newtype Loc = Loc (Int, Int) deriving (Eq, Show)

instance Ord Loc where
  compare (Loc (x1, y1)) (Loc (x2, y2)) = compare (y1, x1) (y2, x2)

type Cave = Set Loc
data Team = Elf | Goblin deriving (Show, Eq, Ord)
data Unit = Unit Loc Team Int deriving (Show, Eq, Ord)

enemy :: Team -> Team
enemy Elf = Goblin
enemy Goblin = Elf

getLoc :: Unit -> Loc
getLoc (Unit l _ _) = l

getTeam :: Unit -> Team
getTeam (Unit _ team _) = team

dead :: Unit -> Bool
dead (Unit _ _ hp) = hp <= 0

getHP :: Unit -> Int
getHP (Unit _ _ hp) = hp

changeHP :: (Int -> Int) -> Unit -> Unit
changeHP how (Unit loc team hp) = Unit loc team (how hp)



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
    oneTeam (Unit _ team _ Seq.:<| rest) = all ((== team) . getTeam) rest

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
        couldAttack = Seq.findIndicesL (\(Unit loc team _) -> getTeam me /= team && (loc `elem` adjacent cave myLoc)) units
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
    moveWhere = head . tail <$> bestPath cave units (getLoc me) (enemy (getTeam me))

    moveTo :: Loc -> Unit -> Unit
    moveTo loc (Unit _ team hp) = Unit loc team hp


bestPath :: Cave -> Seq Unit -> Loc -> Team -> Maybe [Loc]
bestPath cave units from to
  | not targetsExist = Nothing
  | null shortestPaths = Nothing
  | otherwise = Just . minimum $ pathsToBestTarget
  where
    done :: Loc -> Bool
    done loc = isJust $ Seq.findIndexL (\(Unit l team _) -> (team == to) && (l `elem` adjacent cave loc)) units

    shortestPaths :: [[Loc]]
    shortestPaths = bfs (adjacentOpen cave units) done from

    pathsToBestTarget :: [[Loc]]
    pathsToBestTarget = filter ((== bestTarget) . last) shortestPaths
      where bestTarget = minimum $ map last shortestPaths

    targetsExist :: Bool
    targetsExist = any (not . null . adjacentOpen cave units . getLoc) . Seq.filter ((== to) . getTeam) $ units



adjacent :: Cave -> Loc -> [Loc]
adjacent cave (Loc (x, y)) = filter (`Set.member` cave) . fmap Loc $ [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]


adjacentOpen :: Foldable f => Cave -> f Unit -> Loc -> [Loc]
adjacentOpen cave units loc = filter unoccupied $ adjacent cave loc
  where
    unoccupied :: Loc -> Bool
    unoccupied l = all ((/= l) . getLoc) units



---------- pretty printing ----------

prettyWorld :: Cave -> Seq Unit -> String
prettyWorld cave units = go [] [] 0 0 [] (Seq.sort units)
  where
    width  = maximum . map (\(Loc (x, _)) -> x) $ Set.toList cave
    height = maximum . map (\(Loc (_, y)) -> y) $ Set.toList cave

    nextUnitHere :: Loc -> Seq Unit -> Bool
    nextUnitHere _ Seq.Empty = False
    nextUnitHere here (Unit loc _ _ Seq.:<| _) = here == loc

    unitGlyph :: Unit -> String
    unitGlyph (Unit _ Elf _) = "\ESC[1;32mE\ESC[0m"
    unitGlyph (Unit _ Goblin _) = "\ESC[1;31mG\ESC[0m"

    prettyUnit :: Unit -> String
    prettyUnit unit@(Unit _ _ hp) = unitGlyph unit ++ "(" ++ show hp ++ ")"

    go :: [String] -> [String] -> Int -> Int -> [Unit] -> Seq Unit -> String
    go rows partialRow x y unitsInRow remainingUnits
      | y > height+2 = init . unlines . reverse $ rows

      | x > width+1 =
        let fullRow = concat $ reverse partialRow
            newRow = fullRow ++ "          " ++ (unwords . reverse . map prettyUnit $ unitsInRow)
        in go (newRow:rows) [] 0 (y+1) [] remainingUnits

      | nextUnitHere (Loc (x, y)) remainingUnits =
        let unitHere Seq.:<| remainingUnits' = remainingUnits
            partialRow' = unitGlyph unitHere : partialRow
            unitsInRow' = unitHere : unitsInRow
        in go rows partialRow' (x+1) y unitsInRow' remainingUnits'

      | otherwise =
        let newChar = if Set.member (Loc (x, y)) cave then " " else "\ESC[38;5;8m#\ESC[0m"
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
          <|> do { char 'G'; loc <- parserLoc; return (Just loc, Just $ Unit loc Goblin 200) }
          <|> do { char 'E'; loc <- parserLoc; return (Just loc, Just $ Unit loc Elf 200) }

parserLoc :: Parsec String () Loc
parserLoc = do
  sourcePos <- getPosition
  return $ Loc (sourceColumn sourcePos - 2, sourceLine sourcePos - 1)
