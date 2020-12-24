{-# LANGUAGE LambdaCase #-}
module D18 where

import Data.Char (isDigit)

data Token
  = Num Integer
  | Add
  | Mul
  | Open
  | Close
  deriving (Eq, Ord, Show)

parse :: String -> [[Token]]
parse = map parseLine . lines
  where
    parseLine [] = []
    parseLine (' ':rest) = parseLine rest
    parseLine ('(':rest) = Open : parseLine rest
    parseLine (')':rest) = Close : parseLine rest
    parseLine ('+':rest) = Add : parseLine rest
    parseLine ('*':rest) = Mul : parseLine rest
    parseLine line = Num (read (takeWhile isDigit line)) : parseLine (dropWhile isDigit line)

eval :: [Token] -> Integer
eval = go [Left id]
  where
    go :: [Either (Integer -> Integer) Integer] -> [Token] -> Integer
    go [Right n] [] = n
    go (Left f:ops) (Num n:ts) = go (Right (f n) : ops) ts
    go (Right n:ops) (Add:ts) = go (Left (n+) : ops) ts
    go (Right n:ops) (Mul:ts) = go (Left (n*) : ops) ts
    go ops (Open:ts) = go (Left id : ops) ts
    go (Right n : Left f : ops) (Close:ts) = go (Right (f n) : ops) ts
    go ops ts = error $ "Invalid token sequence. Cannot evaluate " ++ show ts ++ " with state " ++ concatMap (\case Left f -> "<closure>"; Right n -> show n) ops


part1 = sum .  map eval


fix :: Eq a => (a -> a) -> a -> a
fix f a = if a == next then a else fix f next
  where next = f a

evalAdds :: [Token] -> [Token]
evalAdds (Num a : Add : Num b : rest) = evalAdds $ Num (a+b) : rest
evalAdds (t:ts) = t : evalAdds ts
evalAdds [] = []

evalParens :: ([Token] -> [Token]) -> [Token] -> [Token]
evalParens recurse [] = []
evalParens recurse (Open:rest) = case outer of
  (Close:outer) -> recurse inner ++ evalParens recurse outer
  (Open:nested) -> Open : inner ++ evalParens recurse outer
  where
    inner = takeWhile (not . (`elem` [Open, Close])) rest
    outer = dropWhile (not . (`elem` [Open, Close])) rest
evalParens recurse (t:ts) = t : evalParens recurse ts

evalMuls :: [Token] -> [Token]
evalMuls (Num a : Mul : Num b : rest) = evalMuls $ Num (a*b) : rest
evalMuls (t:ts) = t : evalMuls ts
evalMuls [] = []

part2 = sum . map (getValue . doEval)
  where
    doEval = evalArith . fix (evalParens evalArith)
    evalArith = evalMuls . evalAdds
    getValue [Num n] = n
    getValue bad = error $ "Not reduced: " ++ show bad


run :: IO ()
run = do
  input <- parse <$> readFile "input/18"
  print $ part1 input
  print $ part2 input
