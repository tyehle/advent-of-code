module Y2018.D12 where


import Control.Monad.Identity
import Data.List (sort, find)
import Text.Parsec
import Text.Parsec.Char


type Rule = ([Bool], Bool)


run :: String -> IO ()
run fileName = do
  (state0, rules) <- unsafeParse parseInput fileName <$> readFile fileName
  print $ crunkStar rules 20 (0, state0)
  -- let allStates = iterate (step rules) (0, state0)
  -- forM_ (take 500 (drop 500 allStates)) $ \(offset, state) -> putStrLn (show offset ++ ": " ++ prettyState state)
  print $ crunkStar rules 50000000000 (0, state0)


prettyState :: [Bool] -> String
prettyState = map showPlant
  where
    showPlant False = '.'
    showPlant True = '#'


match :: [Rule] -> [Bool] -> Bool
match rules window
  | length window /= 5 = error $ "Bad window: " ++ prettyState window
  | otherwise = maybe (error $ "Window not found: " ++ prettyState window) snd $ find ((== window) . fst) rules


step :: [Rule] -> (Integer, [Bool]) -> (Integer, [Bool])
step rules (offset, state) = trim (offset - 2, go [] (pad ++ state ++ pad))
  where
    pad :: [Bool]
    pad = [False, False, False, False]
    trim :: (Integer, [Bool]) -> (Integer, [Bool])
    trim (n, False:rest) = trim (n+1, rest)
    trim (n, startsTrue) = (n, reverse . dropWhile (== False) . reverse $ startsTrue)
    go :: [Bool] -> [Bool] -> [Bool]
    go out input@(ll:l:c:r:rr:rest) = go (match rules [ll,l,c,r,rr] : out) (tail input)
    go out _ = reverse out


sumIndices :: (Integer, [Bool]) -> Integer
sumIndices (offset, state) = sum . map fst . filter snd $ zip [offset..] state


crunkStar :: [Rule] -> Integer -> (Integer, [Bool]) -> Integer
crunkStar _ 0 state = sumIndices state
crunkStar rules fuel (offset, state)
  | state == nextState = sumIndices (offset + fuel*(nextOffset-offset), state)
  | otherwise = crunkStar rules (fuel-1) (nextOffset, nextState)
  where
    (nextOffset, nextState) = step rules (offset, state)




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
