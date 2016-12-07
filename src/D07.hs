module D07 where

import Text.Parsec
import Data.List (group, sort)

data Part = In String | Out String deriving (Show)

isIn :: Part -> Bool
isIn (In _) = True
isIn (Out _) = False

isOut :: Part -> Bool
isOut (In _) = False
isOut (Out _) = True

inner :: Part -> String
inner (In s) = s
inner (Out s) = s

run :: IO ()
run = do
  addresses <- parseFile <$> readFile "resources/d07.txt"
  putStr "TLS count: "
  print . length . filter tls $ addresses
  putStr "SSL count: "
  print . length . filter ssl $ addresses

parseFile :: String -> [[Part]]
parseFile input = either (error . show) id $ parse (many addressParser) "input" input

addressParser :: Parsec String () [Part]
addressParser = many partParser <* newline

partParser :: Parsec String () Part
partParser =   (In <$> between (char '[') (char ']') (many1 letter))
           <|> (Out <$> many1 letter)

containsABBA :: String -> Bool
containsABBA s | length s < 4 = False
               | isABBA s     = True
               | otherwise    = containsABBA (tail s)
  where
    isABBA (a:b:c:d:_) = a == d && b == c && a /= b
    isABBA _ = False

tls :: [Part] -> Bool
tls parts = all noIn parts && any yesOut parts
  where
    noIn (In s) = not (containsABBA s)
    noIn _ = True
    yesOut (Out s) = containsABBA s
    yesOut _ = False

getABAs :: String -> [String]
getABAs s | length s < 3 = []
          | isABA s      = take 3 s : getABAs (tail s)
          | otherwise    = getABAs (tail s)
  where
    isABA (a:b:c:_) = a == c && a /= b
    isABA _ = False

ssl :: [Part] -> Bool
ssl parts = any (\o -> any (\i -> reverseABA i == o) ins) outs
  where
    reverseABA [a,b,_] = [b,a,b]
    reverseABA _ = undefined
    allABAs = map head . group . sort . concatMap getABAs
    outs = allABAs . map inner . filter isOut $ parts
    ins  = allABAs . map inner . filter isIn $ parts
