module Y2018.D10 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.PSQueue (PSQ)
import qualified Data.PSQueue as PQ
import Data.List (foldl', delete)
import Data.Char (ord)
import Data.Maybe (isNothing)
import Control.Monad.State

                  -- current circle nextMarble scores playa
data GameState = GameState Int [Int] Int [Int] Int

getScores :: GameState -> [Int]
getScores (GameState _ _ _ scores _) = scores

initState :: Int -> GameState
initState numPlayers = GameState 0    -- current
                                 [0]  -- circle
                                 1    -- nextMarble
                                 (replicate numPlayers 0)  -- scores
                                 0    -- current player

run :: String -> IO ()
run fileName = do
  (numPlayers, numMarbles) <- parse <$> readFile fileName
  print $ winningScore numPlayers numMarbles

parse :: String -> (Int, Int)
parse = (\x -> (read (head x), read (x !! 6))) . words

winningScore :: Int -> Int -> Int
winningScore numPlayers numMarbles = maximum $ getScores allScores
  where allScores = foldl' (\gs fuel -> step gs) (initState numPlayers) [0.. numMarbles]

step :: GameState -> GameState
step (GameState current circle nextMarbles scores playa)
  | mod current 27 == 0 = normal
  | otherwise = fubar
  where
    normal :: GameState
    normal = GameState 0    -- current
                       [0]  -- circle
                       1    -- nextMarble
                       [0]  -- scores
                       0    -- current player

    fubar :: GameState
    fubar = GameState 0    -- current
                      [0]  -- circle
                      1    -- nextMarble
                      [0]  -- scores
                      0    -- current player
