module D04 where

import Text.Parsec
import Text.Parsec.Char (letter)
import Data.List (group, sort, sortOn)
import Data.Maybe (catMaybes)

type Room = ([String], Integer)

run :: IO ()
run = do
  input <- readFile "resources/d04.txt"
  let rooms = either (error . show) id $ parseAllRooms input
  putStr "Sum of valid sectors: "
  print . sum . map snd . catMaybes $ rooms

parseAllRooms :: String -> Either ParseError [Maybe Room]
parseAllRooms = parse (endBy parseRoom newline) "d04.txt"

parseName :: Parsec String () [String]
parseName = many1 $ many1 letter >>= \word -> char '-' >> return word

parseChecksum :: Parsec String () String
parseChecksum = between (char '[') (char ']') $ count 5 letter

parseRoom :: Parsec String () (Maybe Room)
parseRoom = do
  name <- parseName
  sector <- read <$> many1 digit
  checksum <- parseChecksum
  return $ if isValid name checksum then Just (name, sector) else Nothing

isValid :: [String] -> String -> Bool
isValid name checksum = checksum == expectedChecksum
  where
    frequencies = map (\cs -> (length cs, cs)) . group . sort . concat $ name
    expectedChecksum = take 5 . map (head . snd) . sortOn (\(n, cs) -> (-n, cs)) $ frequencies
