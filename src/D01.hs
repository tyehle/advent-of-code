module D01 where

import Data.Char (isNumber)

data Direction = L | R deriving (Show)
data Heading = North | East | South | West deriving (Show)

type Position = (Integer, Integer)
type Command = (Direction, Integer)
type State = (Heading, Position)

run :: IO ()
run = do
  commands <- getDirections
  let (_, destination) = foldl runCommand (North, (0,0)) commands
  print $ manhattenDistance (0,0) destination

getDirections :: IO [Command]
getDirections = parseDirections <$> readFile "resources/d01_directions.txt"

parseDirections :: String -> [Command]
parseDirections = map getPair . words
  where
    getPair s = (getDir s, getDist s)
    getDir (d:_) | d == 'L' = L
                 | d == 'R' = R
                 | otherwise = error "unexpected direction"
    getDist (_:s) = read . takeWhile isNumber $ s

runCommand :: State -> Command -> State
runCommand (heading, pos) (dir, dist) = (newHeading, newPos)
  where
    newHeading = turn heading dir
    newPos = travel pos newHeading dist

turn :: Heading -> Direction -> Heading
turn North L = West
turn North R = East
turn East  L = North
turn East  R = South
turn South L = East
turn South R = West
turn West  L = South
turn West  R = North

travel :: Position -> Heading -> Integer -> Position
travel (x,y) North n = (x, y+n)
travel (x,y) East  n = (x+n, y)
travel (x,y) South n = (x, y-n)
travel (x,y) West  n = (x-n, y)

manhattenDistance :: Position -> Position -> Integer
manhattenDistance (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)
