{-# LANGUAGE NamedFieldPuns #-}
module Interp where


data Instruction
  = Nop Int
  | Acc Int
  | Jmp Int
  deriving (Eq, Ord, Show)


data State = State
  { pc :: Int
  , acc :: Int
  }


parseProgram :: String -> [Instruction]
parseProgram = map (instruction . words) . lines
  where
    instruction ["nop", num] = Nop $ readNum num
    instruction ["acc", num] = Acc $ readNum num
    instruction ["jmp", num] = Jmp $ readNum num
    instruction bad = error $ "Invalid instruction: " ++ show bad
    readNum = read . dropWhile (== '+')


step :: [Instruction] -> State -> State
step prog State{pc, acc} = case prog !! pc of
  Nop _ -> State (pc+1) acc
  Acc n -> State (pc+1) (acc+n)
  Jmp n -> State (pc+n) acc
