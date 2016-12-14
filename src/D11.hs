module D11 where

import Data.List (find, (\\))
import Data.Maybe (fromJust, fromMaybe)
import Data.Array
import Data.Set (Set)
import qualified Data.Set as Set

run :: IO ()
run = do
  print initialState
  putStrLn $ "Is valid: " ++ show (isValid initialState)
  putStrLn $ "Is done: " ++ show (isDone initialState)
  putStrLn "All moves:"
  mapM_ print . filter isValid . allMoves $ initialState
  print . length $ bfs (Set.empty, [[initialState]])


data Isotope = Tm | Pu | Sr | Pm | Ru deriving (Eq, Show, Ord)
data Entity = Generator Isotope | Chip Isotope | Elevator deriving (Eq, Show, Ord)

isGenerator :: Entity -> Bool
isGenerator Generator{} = True
isGenerator _ = False

isChip :: Entity -> Bool
isChip Chip{} = True
isChip _ = False


type State = Array Int [Entity]

-- The first floor contains a thulium generator, a thulium-compatible microchip, a plutonium generator, and a strontium generator.
-- The second floor contains a plutonium-compatible microchip and a strontium-compatible microchip.
-- The third floor contains a promethium generator, a promethium-compatible microchip, a ruthenium generator, and a ruthenium-compatible microchip.
-- The fourth floor contains nothing relevant.
initialState :: State
initialState = listArray (1,4)
  [ [Generator Tm, Chip Tm, Generator Pu, Generator Sr, Elevator]
  , [Chip Pu, Chip Sr]
  , [Generator Pm, Chip Pm, Generator Ru, Chip Ru]
  , []
  ]

isDone :: State -> Bool
isDone s = and [null (s ! f) | f <- [1..3]]

isValid :: State -> Bool
isValid = all checkFloor
  where
    checkFloor f = not (any isGenerator f) || all (hasPair f) (filter isChip f)
    hasPair f (Chip i) = Generator i `elem` f
    hasPair _ _ = undefined


bfs :: (Set State, [[State]]) -> [State]
bfs (visited, fringe) = fromMaybe recur $ find (isDone . head) fringe
  where
    validMoves = filter (\s -> isValid s && not (Set.member s visited)) . allMoves
    advance history = [s:history | s <- validMoves (head history)]
    recur = bfs (visited `Set.union` Set.fromList (map head fringe), concatMap advance fringe)

allMoves :: State -> [State]
allMoves s = [doMove ents floorTo | ents <- movableItems, floorTo <- adjacentFloors]
  where
    floorNum = fst . fromJust . find (\(_,es) -> Elevator `elem` es) . assocs $ s
    adjacentFloors = filter (\f -> 1 <= f && f <= 4) [floorNum+1, floorNum-1]
    movableItems = map (Elevator:) $ concat [(choice . filter f) (s ! floorNum) | f <- [isGenerator, isChip], choice <- [choose 1, choose 2]]
    doMove ents floorTo = s // [(floorNum, (s!floorNum) \\ ents), (floorTo, (s!floorTo) ++ ents)]

choose :: Int -> [a] -> [[a]]
choose 0 _ = [[]]
choose _ [] = []
choose n (x:xs) = map (x:) (choose (n-1) xs) ++ choose n xs
