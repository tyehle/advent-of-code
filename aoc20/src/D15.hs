module D15 where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


part1 stop input = go (last input) (fromIntegral $ length input) (Map.fromList $ zip (init input) [1..])
  where
    go :: Integer -> Integer -> Map Integer Integer -> Integer
    go prev turn spoken
      | turn == stop = prev
      | otherwise = case Map.lookup prev spoken of
          Nothing -> go 0 (turn + 1) nextSpoken
          Just lastSpokenOn -> go (turn - lastSpokenOn) (turn + 1) nextSpoken
      where
        nextSpoken = Map.insert prev turn spoken


run :: IO ()
run = do
  let input = [2,0,6,12,1,3]
  -- mapM_ print [part1 n [0, 3, 6] | n <- [4..10]]
  print $ part1 2020 input
  print $ part1 30000000 input
