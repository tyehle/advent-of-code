module Y2016.D08 where

import Text.Parsec
import Data.List (intercalate)
import Data.Array

data Command = Rect Int Int | Row Int Int | Col Int Int deriving (Show)

run :: IO ()
run = do
  commands <- parseFile <$> readFile "resources/d08.txt"
  let finalGrid = foldl runCommand emptyGrid commands
  print . length . filter id . elems $ finalGrid
  putStrLn . prettyGrid $ finalGrid


parseFile :: String -> [Command]
parseFile = either (error . show) id . parse (sepEndBy1 commandP newline) "input"

commandP :: Parsec String () Command
commandP =  uncurry Rect <$> prefixPair (word "rect ") (char 'x')
        <|> uncurry Row <$> prefixPair (word "rotate row y=") (word " by ")
        <|> uncurry Col <$> prefixPair (word "rotate column x=") (word " by ")
  where
    word = try . string
    num = read <$> many1 digit
    prefixPair prefix sep = (,) <$> (prefix *> num <* sep) <*> num


width,height :: Int
width = 50
height = 6

-- (col, row)
type Grid = Array (Int,Int) Bool

emptyGrid :: Grid
emptyGrid = listArray gridBounds $ replicate (width*height) False

gridBounds :: ((Int, Int), (Int, Int))
gridBounds = ((0,0), (width-1, height-1))

runCommand :: Grid -> Command -> Grid
runCommand grid command = case command of
  Rect wide tall -> grid // [((c, r), True) | c <- [0..wide-1], r <- [0..tall-1]]
  Row r dist -> grid // [((c, r), grid ! ((c - dist) `mod` width, r)) | c <- [0..width-1]]
  Col c dist -> grid // [((c, r), grid ! (c, (r - dist) `mod` height)) | r <- [0..height-1]]

prettyGrid :: Grid -> String
prettyGrid grid = intercalate "\n" . map (map toChar) $ [[grid ! (c,r) | c <- [0..width-1]] | r <- [0..height-1]]
  where
    toChar b = if b then '#' else ' '
