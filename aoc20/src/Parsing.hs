{-# LANGUAGE FlexibleContexts #-}
module Parsing where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char


parseGrid :: (Num n, Ord n) => String -> Map (n, n) Char
parseGrid = go 0 0 Map.empty
  where
    go _ _ m [] = m
    go _ y m ('\n':cs) = go 0 (y+1) m cs
    go x y m (c:cs) = go (x+1) y (Map.insert (x, y) c m) cs

printGrid :: (Num n, Ord n, Enum n) => Map (n, n) Char -> String
printGrid grid = concat [maybe ' ' id (Map.lookup (x, y) grid) : if x == maxX then "\n" else "" | y <- [minY .. maxY], x <- [minX .. maxX]]
  where
    minX = minimum $ map fst $ Map.keys grid
    minY = minimum $ map snd $ Map.keys grid
    maxX = maximum $ map fst $ Map.keys grid
    maxY = maximum $ map snd $ Map.keys grid


split :: Eq a => [a] -> [a] -> [[a]]
split initialItems sep = go [] initialItems
  where
    go group [] = [reverse group]
    go group items@(next:rest)
      | sep `isPrefixOf` items = reverse group : go [] (drop (length sep) items)
      | otherwise = go (next:group) rest


type Parser a = Parsec Void String a

unsafeParse :: Parser a -> String -> a
unsafeParse parser input = either (error . errorBundlePretty) id $ parse parser "input" input

linesP :: Parser a -> Parser [a]
linesP line = sepEndBy line eol <* eof

digitsP :: (Read n) => Parser n
digitsP = read <$> some digitChar
