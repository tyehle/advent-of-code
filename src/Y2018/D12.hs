module Y2018.D12 where


import Text.Parsec
import Text.Parsec.Char
import Control.Monad.Identity


type Rule = ([Bool], Bool)


run :: String -> IO ()
run fileName = do
  (state0, rules) <- unsafeParse parseInput fileName <$> readFile fileName
  putStrLn $ prettyState state0


prettyState :: [Bool] -> String
prettyState = map showPlant
  where
    showPlant False = '.'
    showPlant True = '#'


prettyRule :: Rule -> String
prettyRule (pat, to) = prettyState pat ++ " => " ++ prettyState [to]


unsafeParse :: Parsec String () a -> SourceName -> String -> a
unsafeParse parser file = either (error . show) id . parse parser file


parseInput :: Parsec String () ([Bool], [Rule])
parseInput = do
  string "initial state: "
  plants <- many1 pot
  newline
  newline
  rules <- many1 (parseRule <* newline)
  eof
  return (plants, rules)


parseRule :: Parsec String () Rule
parseRule = do
  pat <- replicateM 5 pot
  string " => "
  result <- pot
  return (pat, result)


pot :: Parsec String () Bool
pot = isPlant <$> oneOf ".#"
  where
    isPlant :: Char -> Bool
    isPlant '.' = False
    isPlant '#' = True
