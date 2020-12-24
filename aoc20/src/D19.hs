module D19 where

import Data.Map (Map, (!))
import qualified Data.Map as Map

import Parsing (split)

import Debug.Trace


data Rule
  = Leaf Char
  | Node [[Integer]]
  deriving (Eq, Ord, Show)


parse :: String -> (Map Integer Rule, [String])
parse raw = (Map.fromList $ map parseRule rules, inputs)
  where
    [rules, inputs] = flip split [""] $ lines raw
    parseRule line = (read name, tree)
      where
        [name, rule] = split line ": "
        tree = case rule of
          ['"', c, '"'] -> Leaf c
          _ -> Node $ map (map read . flip split " ") $ split rule " | "


match :: Map Integer Rule -> String -> Bool
match rules = ("" `elem`) . matchRule (rules ! 0)
  where
    matchRule :: Rule -> String -> [String]
    matchRule _ [] = []
    matchRule (Leaf rule) (c:chars)
      | rule == c = [chars]
      | otherwise = []
    matchRule (Node sumOfProducts) chars = do
      prod <- sumOfProducts
      matchConcat (map (rules !) prod) chars

    matchConcat [] chars = [chars]
    matchConcat (r:rs) chars = do
      cs <- matchRule r chars
      matchConcat rs cs


part1 (rules, inputs) = length $ filter (match rules) inputs


part2 (initialRules, inputs) = length $ filter (match rules) inputs
  where
    newRules = Map.fromList [(8, Node [[42], [42, 8]]), (11, Node [[42, 31], [42, 11, 31]])]
    rules = Map.union newRules initialRules


run :: IO ()
run = do
  input <- parse <$> readFile "input/19"
  print $ part1 input
  print $ part2 input

test = "0: 4 1 5\n\
       \1: 2 3 | 3 2\n\
       \2: 4 4 | 5 5\n\
       \3: 4 5 | 5 4\n\
       \4: \"a\"\n\
       \5: \"b\"\n\
       \\n\
       \ababbb\n\
       \bababa\n\
       \abbbab\n\
       \aaabbb\n\
       \aaaabbb"
