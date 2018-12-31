module Y2018.D25 where

import Data.List (partition, delete)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Parsec

import ParseUtil


type Loc = [Int]


run :: String -> IO ()
run fileName = do
  points <- unsafeParse parseInput fileName <$> readFile fileName
  print $ part1 points


part1 :: [Loc] -> Int
part1 = length . connectedComponents . mkGraph


l1 :: Loc -> Loc -> Int
l1 a b = sum . map abs $ zipWith (-) a b


mkGraph :: [Loc] -> Map Loc [Loc]
mkGraph points = Map.fromList [(p,filter ((<= 3) . l1 p) (delete p points)) | p <- points]


connectedComponents :: Map Loc [Loc] -> [Set Loc]
connectedComponents graph = go [] $ Map.keys graph
  where
    go components [] = components
    go components (v:vs) = go (Set.insert v (Set.unions connected) : others) vs
      where
        (connected, others) = partition (\c -> any (`Set.member` c) (graph ! v)) components


---------- parsing ----------

parseInput :: Parsec String () [Loc]
parseInput = sepEndBy parsePoint newline <* eof

parsePoint :: Parsec String () Loc
parsePoint = do
  a <- int <* string ","
  b <- int <* string ","
  c <- int <* string ","
  d <- int
  return [a, b, c, d]
