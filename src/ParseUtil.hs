module ParseUtil
  ( unsafeParse
  , nat
  ) where


import Text.Parsec


unsafeParse :: Parsec String () a -> SourceName -> String -> a
unsafeParse parser file = either (error . show) id . parse parser file


nat :: Parsec String a Int
nat = read <$> many1 digit
