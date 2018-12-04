module Y2018.D02 where

import qualified Data.Map as Map
import Data.List




run :: IO ()
run = do
  fileName <- getLine
  ids <- lines <$> readFile fileName
  print $ checksum ids


checksum :: [String] -> Int
checksum ids = (count . map fst $ freaks) * (count . map snd $ freaks)
  where freaks = map frequencies ids

-- thanks SO
count list = sum $ map fromEnum list


frequencies :: Ord a => [a] -> (Bool, Bool)
frequencies input = (2 `elem` counts, 3 `elem` counts)
  where counts = map length . group . sort $ input
