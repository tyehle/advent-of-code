module Y2018.D12 where


import Control.Monad.Identity
import Data.List (sort, find)
import Text.Parsec
import Text.Parsec.Char


type Rule = ([Bool], Bool)


run :: String -> IO ()
run fileName = do
  (state0, rules) <- unsafeParse parseInput fileName <$> readFile fileName
  -- let allStates = iterate (step rules) (0, state0)
  print $ crunkStar rules 20 (0, state0)
  -- print . sumIndices $ allStates !! 20
  print $ crunkStar rules 500000 (0, state0)


prettyState :: [Bool] -> String
prettyState = map showPlant
  where
    showPlant False = '.'
    showPlant True = '#'


match :: [Rule] -> [Bool] -> Bool
match rules window
  | length window /= 5 = error $ "Bad window: " ++ prettyState window
  | otherwise = maybe (error $ "Window not found: " ++ prettyState window) snd $ find ((== window) . fst) rules


step :: [Rule] -> (Int, [Bool]) -> (Int, [Bool])
step rules (offset, state) = trim (offset - 2, go [] (pad ++ state ++ pad))
  where
    pad :: [Bool]
    pad = [False, False, False, False]
    trim :: (Int, [Bool]) -> (Int, [Bool])
    trim (n, False:rest) = trim (n+1, rest)
    trim (n, startsTrue) = (n, reverse . dropWhile (== False) . reverse $ startsTrue)
    go :: [Bool] -> [Bool] -> [Bool]
    go out input@(ll:l:c:r:rr:rest) = go (match rules [ll,l,c,r,rr] : out) (tail input)
    go out _ = reverse out


sumIndices :: (Int, [Bool]) -> Int
sumIndices (offset, state) = sum . map fst . filter snd $ zip [offset..] state


crunkStar :: [Rule] -> Integer -> (Int, [Bool]) -> Int
crunkStar _ 0 state = sumIndices state
crunkStar rules n state = crunkStar rules (n-1) $ step rules state


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
