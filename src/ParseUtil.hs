module ParseUtil
  ( unsafeParse
  ) where


import Text.Parsec


unsafeParse :: Parsec String () a -> SourceName -> String -> a
unsafeParse parser file = either (error . show) id . parse parser file
