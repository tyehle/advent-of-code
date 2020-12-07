module D04 where

import Data.Char
import Data.List ( isSuffixOf )
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Read (readMaybe)

import Text.Megaparsec ( noneOf, sepEndBy, some, eof )
import Text.Megaparsec.Char

import Parsing

parse :: String -> [Map String String]
parse = unsafeParse (sepEndBy parseObj (string "\n") <* eof)
  where
    parseObj = Map.fromList <$> sepEndBy entry spaceChar
    entry = do
      key <- some (noneOf "\n :")
      char ':'
      value <- some (noneOf "\n :")
      return (key, value)

part1 = length . filter isValid
  where
    isValid m = all (flip Map.member m) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

validators :: [(String, String -> Bool)]
validators =
  [ ("byr", yearRange 1920 2002)
  , ("iyr", yearRange 2010 2020)
  , ("eyr", yearRange 2020 2030)
  , ("hgt", height)
  , ("hcl", \c -> length c == 7 && head c == '#' && all isHexDigit (drop 1 c))
  , ("ecl", flip elem ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])
  , ("pid", \i -> length i == 9 && all isDigit i)
  ]
  where
    yearRange low high input = length input == 4 && inRange low high input
    height h
      | "cm" `isSuffixOf` h = inRange 150 193 (reverse $ drop 2 $ reverse h)
      | "in" `isSuffixOf` h = inRange 59 76 (reverse $ drop 2 $ reverse h)
      | otherwise = False
    inRange low high = maybe False (\n -> low <= n && n <= high) . readMaybe

part2 = length . filter isValid
  where
    isValid m = all (check m) validators
    check m (k, p) = maybe False p $ Map.lookup k m

run :: IO ()
run = do
  input <- parse <$> readFile "input/04"
  print $ part1 input
  print $ part2 input