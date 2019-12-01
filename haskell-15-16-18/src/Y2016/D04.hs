module Y2016.D04 where

import Text.Parsec
import Text.Parsec.Char (letter)
import Data.List (group, sort, sortOn)
import Data.Maybe (catMaybes)
import Data.Char (ord, chr)

type Room = ([String], Int)

run :: IO ()
run = do
  input <- readFile "resources/d04.txt"
  let validRooms = catMaybes . either (error . show) id $ parseAllRooms input
  putStr "Sum of valid sectors: "
  print . sum . map snd $ validRooms
  putStr "northpole entry: "
  print . filter (\(name, _) -> elem "northpole" name) . map decrypt $ validRooms


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

decrypt :: Room -> Room
decrypt (name, sector) = (map (map (cycleChar sector)) name, sector)

cycleChar :: Int -> Char -> Char
cycleChar n = toChar . (+n) . toInt
  where
    toInt = subtract 97 . ord
    toChar i = chr $ (i `mod` 26) + 97
