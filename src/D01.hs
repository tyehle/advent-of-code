module D01 where

import Data.Maybe (fromJust)
import Data.Char (isNumber)
import Data.Set (Set)
import qualified Data.Set as Set

data Direction = L | R deriving (Show)
data Heading = North | East | South | West deriving (Show)

data Command = Turn Direction | Move Integer deriving (Show)

type Position = (Integer, Integer)
type State = (Heading, Position)

run :: IO ()
run = do
  commands <- getDirections
  let positions = map snd $ runCommands commands (North, (0,0))
  putStr "Distance to final location: "
  print $ manhattenDistance (0,0) (last positions)
  putStr "Distance to first revisit: "
  print $ fmap (manhattenDistance (0,0)) (firstDuplicate positions)

firstDuplicate :: Ord a => [a] -> Maybe a
firstDuplicate = checkRemaining Set.empty
  where
    checkRemaining _ [] = Nothing
    checkRemaining seen (x:xs) = if Set.member x seen
                                 then Just x
                                 else checkRemaining (Set.insert x seen) xs

getDirections :: IO [Command]
getDirections = parseDirections <$> readFile "resources/d01_directions.txt"

parseDirections :: String -> [Command]
parseDirections = concatMap getPair . words
  where
    getPair s = [getDir s, getDist s]
    getDir (d:_) | d == 'L' = Turn L
                 | d == 'R' = Turn R
                 | otherwise = error "unexpected direction"
    getDist (_:s) = Move . read . takeWhile isNumber $ s

runCommands :: [Command] -> State -> [State]
runCommands [] s = [s]
runCommands (Turn dir : cs) s@(heading, pos) = runCommands cs (turn heading dir, pos)
runCommands (Move 0 : cs) s = runCommands cs s
runCommands (Move n : cs) s@(heading, pos) = s : runCommands (Move (n-1) : cs) (heading, step pos heading)

turn :: Heading -> Direction -> Heading
turn North L = West
turn North R = East
turn East  L = North
turn East  R = South
turn South L = East
turn South R = West
turn West  L = South
turn West  R = North

step :: Position -> Heading -> Position
step (x,y) North = (x, y+1)
step (x,y) East  = (x+1, y)
step (x,y) South = (x, y-1)
step (x,y) West  = (x-1, y)

manhattenDistance :: Position -> Position -> Integer
manhattenDistance (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)
