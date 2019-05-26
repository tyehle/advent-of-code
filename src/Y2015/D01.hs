module Y2015.D01 where

import Data.Foldable (foldl')

run :: String -> IO ()
run fileName = do
  input <- readFile fileName
  print $ foldl' moveSanta 0 input
  print $ elemIndex (-1) $ scanl moveSanta 0 input

moveSanta :: Int -> Char -> Int
moveSanta acc x = acc + directions x
  where
    directions '(' = 1
    directions ')' = -1
    directions _   = 0
