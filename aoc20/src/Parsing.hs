{-# LANGUAGE FlexibleContexts #-}
module Parsing where

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


type Parser a = Parsec Void String a

unsafeParse :: Parser a -> String -> a
unsafeParse parser input = either (error . errorBundlePretty) id $ parse parser "input" input

linesP :: Parser a -> Parser [a]
linesP line = sepEndBy line eol <* eof

digitsP :: (Read n) => Parser n
digitsP = read <$> some digitChar
