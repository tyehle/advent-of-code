module Y2018.D09 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (foldl', delete)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

                  -- current circle nextMarble scores playa
data GameState = GameState Int (Seq Int) Int (Map Int Int) Int deriving(Show)

getScores :: GameState -> Map Int Int
getScores (GameState _ _ _ scores _) = scores

initState :: Int -> GameState
initState numPlayers = GameState 0    -- current
                                 (Seq.singleton 0)  -- circle
                                 1    -- nextMarble
                                 (Map.fromList $ zip [0..numPlayers-1] (repeat 0)) -- scores
                                 0    -- current player

run :: String -> IO ()
run fileName = do
  (numPlayers, numMarbles) <- parse <$> readFile fileName
  print $ winningScore numPlayers numMarbles

parse :: String -> (Int, Int)
parse = (\x -> (read (head x), read (x !! 6))) . words

winningScore :: Int -> Int -> Int
winningScore numPlayers numMarbles = maximum . map snd . Map.toList . getScores $ finalState
  where finalState = foldl' (\gs fuel -> step numPlayers gs) (initState numPlayers) [0..numMarbles]

step :: Int -> GameState -> GameState
step numPlayers (GameState current circle nextMarble scores playa)
  | nextMarble `mod` 23 /= 0 = normal
  | otherwise = fubar
  where
    nextPlaya = mod (playa + 1) numPlayers

    normal :: GameState
    normal = GameState next          -- current
                       newCircle     -- circle
                       (nextMarble+1)  -- nextMarble
                       scores        -- scores
                       nextPlaya     -- current player
      where next = mod (current+2) (length circle)
            newCircle = Seq.insertAt next nextMarble circle

    fubar :: GameState
    fubar = GameState (whereToDelete `mod` length newCircle)    -- current
                      newCircle  -- circle
                      (nextMarble+1)    -- nextMarble
                      newScores  -- scores
                      nextPlaya  -- current player
      where points = nextMarble + (circle `Seq.index` whereToDelete)
            newScores = Map.adjust (+ points) playa scores
            whereToDelete = mod (current - 7) (length circle)
            newCircle = Seq.deleteAt whereToDelete circle

-- insertInto :: Int -> Int -> [Int] -> [Int]
-- insertInto what index into = take index into ++ what : drop index into

-- deleteAt :: Int -> [Int] -> [Int]
-- deleteAt index into = take index into ++ drop (index + 1) into
