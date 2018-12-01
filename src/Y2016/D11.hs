module Y2016.D11 where

import Data.List (find, (\\))
import Data.Maybe (fromJust)
import Data.Array
import Search (aStar)

run :: IO ()
run = do
  let path = aStarPath initialState
  print . subtract 1 . length $ path
  mapM_ (print . (\s -> (s, howClose s))) (reverse path)


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

-- The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.
-- The second floor contains a hydrogen generator.
-- The third floor contains a lithium generator.
-- The fourth floor contains nothing relevant.
-- initialState :: State
-- initialState = listArray (1,4)
--   [ [Elevator, Chip Pu, Chip Sr]
--   , [Generator Pu]
--   , [Generator Sr]
--   , []
--   ]


isDone :: State -> Bool
isDone s = and [null (s ! f) | f <- [1..3]]

isValid :: State -> Bool
isValid = all checkFloor
  where
    checkFloor f = not (any isGenerator f) || all (hasPair f) (filter isChip f)
    hasPair f (Chip i) = Generator i `elem` f
    hasPair _ _ = undefined

howClose :: State -> Int
howClose s = movesForElevator + movesToFloor 4
  where
    movesForElevator = fromJust (findIndex (elem Elevator) s) - lowestOccupiedFloor
    lowestOccupiedFloor = fromJust . findIndex (not . null) $ s
    itemCount floorNum = let baseNum = foldl (\t e -> if e == Elevator then t-1 else t+1) 0 (s!floorNum)
                         in if floorNum == lowestOccupiedFloor then baseNum + 1 else baseNum
    movesToFloor n | n <= lowestOccupiedFloor = 0
                   | otherwise = movesToFloor (n-1) + sum (map itemCount [lowestOccupiedFloor..n-1]) - 1

findIndex :: Ix i => (e -> Bool) -> Array i e -> Maybe i
findIndex c xs = fst <$> find (\(_,x) -> c x) (assocs xs)


aStarPath :: State -> [State]
aStarPath = aStar (filter isValid . allMoves) howClose isDone

allMoves :: State -> [State]
allMoves s = [doMove ents floorTo | ents <- movableItems, floorTo <- adjacentFloors]
  where
    floorNum = fromJust . findIndex (elem Elevator) $ s
    adjacentFloors = filter (\f -> 1 <= f && f <= 4) [floorNum+1, floorNum-1]
    movableItems = map (Elevator:) . (\ents -> choose 1 ents ++ choose 2 ents) . filter (/= Elevator) $ s ! floorNum
    doMove ents floorTo = s // [(floorNum, (s!floorNum) \\ ents), (floorTo, (s!floorTo) ++ ents)]

choose :: Int -> [a] -> [[a]]
choose 0 _ = [[]]
choose _ [] = []
choose n (x:xs) = map (x:) (choose (n-1) xs) ++ choose n xs
