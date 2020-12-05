module Parsing where

import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char

type Parser a = Parsec Void String a

unsafeParse :: Parser a -> String -> a
unsafeParse parser input = either (error . errorBundlePretty) id $ parse parser "input" input

linesP :: Parser a -> Parser [a]
linesP line = sepEndBy line eol <* eof

digitsP :: (Read n) => Parser n
digitsP = read <$> some digitChar
