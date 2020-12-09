{-# LANGUAGE NamedFieldPuns #-}
module D08 where

import Data.Maybe

data State = State
  { pc :: Int
  , acc :: Int
  }

parse :: String -> [(String, Int)]
parse = map (instruction . words) . lines
  where
    instruction [name, num] = (name, read (dropWhile (== '+') num))

step :: [(String, Int)] -> State -> State
step prog State{pc, acc} = case prog !! pc of
  ("nop", _) -> State (pc+1) acc
  ("acc", n) -> State (pc+1) (acc+n)
  ("jmp", n) -> State (pc+n) acc
  (i, _) -> error $ "Unknown instruction: " ++ i

part1 prog = go [] (State 0 0)
  where
    go visited state
      | pc state `elem` visited = acc state
      | otherwise = go (pc state : visited) (step prog state)

part2 initialProg = head $ mapMaybe (go [] (State 0 0)) (subs initialProg)
  where
    subs [] = []
    subs (i@("jmp", n):rest) = (("nop", n):rest) : map (i:) (subs rest)
    subs (i@("nop", n):rest) = (("jmp", n):rest) : map (i:) (subs rest)
    subs (i:rest) = map (i:) (subs rest)
    go visited s@(State pc acc) prog
      | pc == length prog = Just acc
      | pc `elem` visited = Nothing
      | 0 <= pc && pc < length prog = go (pc:visited) (step prog s) prog
      | otherwise = Nothing

run :: IO ()
run = do
  input <- parse <$> readFile "input/08"
  print $ part1 input
  print $ part2 input