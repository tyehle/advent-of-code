module D06 where

import Data.List (transpose, group, sort, sortOn)

run :: IO ()
run = do
  input <- lines <$> readFile "resources/d06.txt"
  let charsByFreq = map (map head . sortOn length . group . sort) . transpose $ input
  putStr "Message: "
  putStrLn . map last $ charsByFreq
  putStr "Least likely message: "
  putStrLn . map head $ charsByFreq
