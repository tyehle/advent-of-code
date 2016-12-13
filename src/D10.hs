module D10 where

import Text.Parsec
import Data.List (group, sort, find, sortOn)
import Data.Array


run :: IO ()
run = do
  relations <- parseInstructions <$> readFile "resources/d10.txt"
  let inputs = findInputs relations
  print . find (\e -> inputs ! e == [17, 61]) . entities $ relations
  print . product . map (head . (inputs !) . Output) $ [0,1,2]


data Entity = Bot Int | Output Int deriving (Eq, Show, Ord)
data Relation = Take Int Entity | Give Entity Choice Entity deriving (Eq, Show)
data Choice = High | Low deriving (Eq, Show)

instance Ix Entity where
  -- range :: (a, a) -> [a]
  range (low, high) = concatMap (\i -> [Bot i, Output i]) [getNumber low..getNumber high]
  -- index :: (a, a) -> a -> Int
  index (low, _) (Bot n) = (n - getNumber low) * 2
  index (low, _) (Output n) = (n - getNumber low) * 2 + 1
  -- inRange :: (a, a) -> a -> Bool
  inRange (low, high) obj = let n = getNumber obj in getNumber low <= n && getNumber high >= n

getNumber :: Entity -> Int
getNumber (Bot i) = i
getNumber (Output i) = i


entities :: [Relation] -> [Entity]
entities = map head . group . sort . concatMap forRel
  where
    forRel (Take _ e) = [e]
    forRel (Give e1 _ e2) = [e1,e2]

findInputs :: [Relation] -> Array Entity [Int]
findInputs rs = let ins = array (head es, last es) [(e, recurse ins e) | e <- es] in ins
  where
    es = sortOn getNumber . entities $ rs
    recurse ins e = sort . map (givesWhat ins) . filter ((==e) . givesTo) $ rs
    givesWhat _ (Take i _) = i
    givesWhat ins (Give source High _) = maximum $ ins ! source
    givesWhat ins (Give source Low  _) = minimum $ ins ! source

givesTo :: Relation -> Entity
givesTo (Take _ e) = e
givesTo (Give _ _ e) = e


parseInstructions :: String -> [Relation]
parseInstructions = concat . either (error . show) id . parse (sepEndBy instructionP newline) "input"

instructionP :: Parsec String () [Relation]
instructionP = giveP <|> takeP
  where
    word = try . string
    number = read <$> many1 digit
    entityP = (Bot <$> (word "bot " *> number)) <|> (Output <$> (word "output " *> number))
    takeP = pure <$> (Take <$> (word "value " *> number) <*> (word " goes to " *> entityP))
    giveP = do
      who <- entityP
      low <- word " gives low to " *> entityP
      high <- word " and high to " *> entityP
      return [Give who Low low, Give who High high]
