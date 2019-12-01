module Y2018.D05 where

import Data.Char
import qualified Data.Set as Set

run :: String -> IO ()
run fileName = do
  -- parse the input into a record
  polymer <- init <$> readFile fileName
  let answer = reduce polymer
  print . length $ answer
  print $ stickyStar answer

xor :: Bool -> Bool -> Bool
xor False True = True
xor True False = True
xor _ _ = False

-- [1518-08-08 00:45] falls asleep
reduce :: String -> String
reduce x = let next = reduceRec "" x in if x == next then x else reduce next
    where
        reduceRec :: String -> String -> String
        reduceRec done (x1:x2:xs)
            | (isUpper x1 `xor` isUpper x2) && (toLower x1 == toLower x2) = reduceRec done xs
            | otherwise = reduceRec (x1:done)  (x2:xs)
        reduceRec done remaining = reverse done ++ remaining

deletePair :: Char -> String -> String
deletePair c = filter ((/= toLower c) . toLower)

stickyStar :: String -> Int
stickyStar polymer = minimum possibilities
    where
        alphabet = Set.fromList . map toLower $ polymer
        possibilities = map (\c -> length $ reduce $ deletePair c polymer) $ Set.toList alphabet
