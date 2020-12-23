{-# LANGUAGE LambdaCase #-}
module D16 where

import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

import Parsing (split)


type Rule = (String, (Int, Int), (Int, Int))
type Ticket = [Int]


parse :: String -> ([Rule], Ticket, [Ticket])
parse input = (map parseRule ruleLines, parseTicket yourTicketLine, map parseTicket ticketLines)
  where
    [ruleLines, [_, yourTicketLine], _:ticketLines] = split (lines input) [""]
    parseRule line = (name, parseRange a, parseRange b)
      where
        [name, constraints] = split line ": "
        [a, b] = split constraints " or "
    parseRange string = (read begin, read end)
      where [begin, end] = split string "-"
    parseTicket line = map read $ split line ","


ruleToSet :: Rule -> Set Int
ruleToSet (_, (b1, e1), (b2, e2)) = Set.fromList $ [b1..e1] ++ [b2..e2]


allValid :: [Rule] -> Set Int
allValid = foldl' addRule Set.empty
  where
    addRule valid rule = Set.union valid $ ruleToSet rule

part1 :: ([Rule], Ticket, [Ticket]) -> Int
part1 (rules, _, tickets) = foldl' (\total value -> if Set.member value valid then total else value + total) 0 $ concat tickets
  where valid = allValid rules


fix :: Eq a => (a -> a) -> a -> a
fix f x = if next == x then x else fix f next
  where next = f x


part2 :: ([Rule], Ticket, [Ticket]) -> Int
part2 (rules, you, tickets) = product $ map snd $ filter (isPrefixOf "departure" . fst) $ zip keys you
  where
    initialPositionPossibilities :: [[String]]
    initialPositionPossibilities = map (map fst . possibleRules) (transpose validTickets)
      where
        validTickets = you : filter (all (`Set.member` valid)) tickets
          where valid = allValid rules
        possibleRules values = filter (\(_, possible) -> all (`Set.member` possible) values) ruleSets
          where ruleSets = [(name, ruleToSet rule) | rule@(name, _, _) <- rules]

    removeFound :: [[String]] -> [[String]]
    removeFound possibilities = map update possibilities
      where
        found = map head $ filter ((== 1) . length) possibilities
        update names
          | length names == 1 = names
          | otherwise = filter (not . (`elem` found)) names

    keys :: [String]
    keys = map (\case [name] -> name; bad -> error (show bad)) $ fix removeFound initialPositionPossibilities


run :: IO ()
run = do
  input <- parse <$> readFile "input/16"
  print $ part1 input
  print $ part2 input
