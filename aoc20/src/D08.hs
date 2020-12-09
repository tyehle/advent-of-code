module D08 where

import Data.Maybe

import Interp


part1 prog = go [] (State 0 0)
  where
    go visited state
      | pc state `elem` visited = acc state
      | otherwise = go (pc state : visited) (step prog state)

part2 initialProg = head $ mapMaybe (go [] (State 0 0)) (subs initialProg)
  where
    subs [] = []
    subs (i@(Jmp n):rest) = (Nop n:rest) : map (i:) (subs rest)
    subs (i@(Nop n):rest) = (Jmp n:rest) : map (i:) (subs rest)
    subs (i:rest) = map (i:) (subs rest)
    go visited s@(State pc acc) prog
      | pc == length prog = Just acc
      | pc `elem` visited = Nothing
      | 0 <= pc && pc < length prog = go (pc:visited) (step prog s) prog
      | otherwise = Nothing

run :: IO ()
run = do
  input <- parseProgram <$> readFile "input/08"
  print $ part1 input
  print $ part2 input