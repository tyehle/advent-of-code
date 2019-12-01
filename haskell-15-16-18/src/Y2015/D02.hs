module Y2015.D02 where

import Data.List (sort)
import Data.List.Split (splitOn)

run :: String -> IO ()
run fileName = do
  input <- readFile fileName
  let presents = map parseLine $ lines input
  print . sum $ map area presents
  print . sum $ map roban presents


parseLine :: String -> [Int]
parseLine line = sort $ map read (splitOn "x" line)

area :: [Int] -> Int
area [a,b,c] = 3*a*b + 2*b*c + 2*a*c

roban :: [Int] -> Int
roban [a,b,c] = 2*(a+b) + a*b*c
