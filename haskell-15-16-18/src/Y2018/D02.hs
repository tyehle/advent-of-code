module Y2018.D02 where

import Data.List
import Data.Maybe (catMaybes)

run :: String -> IO ()
run fileName = do
  ids <- lines <$> readFile fileName
  print $ checksum ids
  print $ spicyStar ids


checksum :: [String] -> Int
checksum ids = (count id . map fst $ freaks) * (count id . map snd $ freaks)
  where freaks = map frequencies ids

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p


frequencies :: Ord a => [a] -> (Bool, Bool)
frequencies input = (2 `elem` counts, 3 `elem` counts)
  where counts = map length . group . sort $ input

spicyStar :: [String] -> Maybe String
spicyStar xs = getMatching <$> goodPairHuh
  where
    getMatching pair = catMaybes $ uncurry (zipWith (\a b -> if a == b then Just a else Nothing)) pair
    goodPairHuh :: Maybe (String, String)
    goodPairHuh = find ((== 1) . uncurry distance) pairs
    pairs :: [(String, String)]
    pairs = [(a, b) | a <- xs, b <- xs]
    distance :: String -> String -> Int
    distance a b = count id $ zipWith (/=) a b
