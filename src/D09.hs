module D09 where

import Text.Parsec

run :: IO ()
run = do
  input <- readFile "resources/d09.txt"
  print . length . singleDecompress $ input
  print . decompressedSize $ input


singleDecompress :: String -> String
singleDecompress = either (error . show) id . parse parser "input"
  where
    parser = concat <$> alternating (many upper) marker
    marker = markerP (`count` anyChar) (\rep -> concat . replicate rep)

decompressedSize :: String -> Int
decompressedSize = either (error . show) id . parse parser "input"
  where
    parser = sum <$> alternating (length <$> many upper) marker
    marker = markerP (\len -> decompressedSize <$> count len anyChar) (*)

alternating :: Parsec s u a -> Parsec s u a -> Parsec s u [a]
alternating a b = try normalCase <|> try (pure <$> a) <|> return []
  where
    normalCase = do
      x <- try a
      y <- try b
      rest <- alternating a b
      return $ x:y:rest

markerP :: (Int -> Parsec String () a) -> (Int -> a -> b) -> Parsec String () b
markerP bodyP agg = do
  _ <- char '('
  len <- read <$> many1 digit
  _ <- char 'x'
  rep <- read <$> many1 digit
  _ <- char ')'
  body <- bodyP len
  return . agg rep $ body
