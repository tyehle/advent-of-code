module D03 where

import Data.List (sort, transpose)

run :: IO ()
run = do
  input <- readFile "resources/d03.txt"
  putStr "Valid triangles: "
  print . length . filter isTriangle . parseFile $ input
  putStr "Valid vertical triangles: "
  print . length . filter isTriangle . parseVertical $ input

parseFile :: String -> [[Integer]]
parseFile = map (sort . map read . words) . lines

parseVertical :: String -> [[Integer]]
parseVertical = map sort . stride 3 . concat . transpose . toArray
  where
    toArray = map (map read . words) . lines

stride :: Int -> [a] -> [[a]]
stride _ [] = []
stride n xs = take n xs : stride n (drop n xs)

isTriangle :: [Integer] -> Bool
isTriangle [a,b,c] = a+b > c
isTriangle _ = undefined
