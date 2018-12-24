{-# LANGUAGE ScopedTypeVariables #-}

module Y2018.D19 where


import Data.Bits ((.&.), (.|.))
import Data.Functor (($>))
import Data.List (foldl', (\\))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Text.Parsec
import Text.Parsec.Char

import ParseUtil


type IData = (Int, Int, Int)
type Sample = ([Int], (Int, IData), [Int])


data Instruction = Addr | Addi
                 | Mulr | Muli
                 | Banr | Bani
                 | Borr | Bori
                 | Setr | Seti
                 | Gtir | Gtri | Gtrr
                 | Eqir | Eqri | Eqrr
                 deriving (Eq, Show)


run :: String -> IO ()
run fileName = do
  (ip, program) <- unsafeParse parseInput fileName <$> readFile fileName
  print . head $ sheepStar ip program [0, 0, 0, 0, 0, 0]
  print . head $ sheepStar ip program [1, 0, 0, 0, 0, 0]


sheepStar :: Int -> Seq (Instruction, IData) -> [Int] -> [Int]
sheepStar ipReg instructions = go 0
  where
    go :: Int -> [Int] -> [Int]
    go ip regs = case Seq.lookup ip instructions of
      Nothing -> regs
      Just (instruction, idata) -> let postRegs = interp instruction idata preRegs
                                     in go ((postRegs !! ipReg) + 1) postRegs
      where
        preRegs = set ipReg ip regs


{-
          0     addi    IP    16    IP        -- jump init
start:
          1     seti    1           %5        -- %5 = 1
loop1:
          2     seti    1           %3        -- %3 = 1                         %3, %5 = 1
loop2:
          3     mulr    %5    %3    %4        -- %4 = %5 * %3                   %4 = %3 * %5

          4     eqrr    %4    %1    %4        -- %4 = %4 == %1
          5     addr    %4    IP    IP        -- if %4 == %1 then %0 += 5       if %4 == %1 then %0 += %5
          6     addi    IP    1     IP        -- jump 8
          7     addr    %5    %0    %0        -- %0 += %5

          8     addi    %3    1     %3        -- %3 += 1                        %3 ++

          9     gtrr    %3    %1    %4        --                                if %3 <= %1 then  jump loop2
          10    addr    IP    %4    IP        -- jump 12
          11    seti    2           IP        -- jump 3

          12    addi    %5    1     %5        -- %5 += 1                        %5 ++

          13    gtrr    %5    %1    %4        --                                if %5 > %1 then exit else jump loop1
          14    addr    %4    IP    IP        --
          15    seti    1           IP        -- jump 2
          16    mulr    IP    IP    IP        -- exit
init:
          17    addi    %1    2     %1        -- %1 += 2
          18    mulr    %1    %1    %1        -- %1 = %1 ^ 2
          19    mulr    IP    %1    %1        -- %1 *= 19
          20    muli    %1    11    %1        -- %1 *= 11
          21    addi    %4    3     %4        -- %4 += 3
          22    mulr    %4    IP    %4        -- %4 *= 22
          23    addi    %4    7     %4        -- %4 += 7
          24    addr    %1    %4    %1        -- %1 += %4
          25    addr    IP    %0    IP        -- jump IP+%0+1                   if %0 == 0 then jump 1
          26    seti    0           IP        -- jump 1
          27    setr    IP          %4        -- %4 = 27
          28    mulr    %4    IP    %4        -- %4 *= 28
          29    addr    IP    %4    %4        -- %4 += 29
          30    mulr    IP    %4    %4        -- %4 *= 30
          31    muli    %4    14    %4        -- %4 *= 14
          32    mulr    %4    IP    %4        -- %4 *= 32
          33    addr    %1    %4    %1        -- %1 += %4
          34    seti    0           %0        -- %0 = 0
          35    seti    0           IP        -- jump 1
-}


set :: forall a. Int -> a -> [a] -> [a]
set index what = go [] index
  where
    go :: [a] -> Int -> [a] -> [a]
    go done 0 (_:xs) = reverse done ++ what:xs
    go _ _ [] = error "set: out of bounds"
    go done n (x:xs) = go (x:done) (n-1) xs


interp :: Instruction -> IData -> [Int] -> [Int]
interp Addr (a, b, c) regs = set c (regs !! a  +  regs !! b) regs
interp Addi (a, b, c) regs = set c (regs !! a  +  b        ) regs
interp Mulr (a, b, c) regs = set c (regs !! a  *  regs !! b) regs
interp Muli (a, b, c) regs = set c (regs !! a  *  b        ) regs
interp Banr (a, b, c) regs = set c (regs !! a .&. regs !! b) regs
interp Bani (a, b, c) regs = set c (regs !! a .&. b        ) regs
interp Borr (a, b, c) regs = set c (regs !! a .|. regs !! b) regs
interp Bori (a, b, c) regs = set c (regs !! a .|. b        ) regs
interp Setr (a, _, c) regs = set c (regs !! a) regs
interp Seti (a, _, c) regs = set c a regs
interp Gtir (a, b, c) regs = set c (fromEnum (a         > regs !! b)) regs
interp Gtri (a, b, c) regs = set c (fromEnum (regs !! a > b        )) regs
interp Gtrr (a, b, c) regs = set c (fromEnum (regs !! a > regs !! b)) regs
interp Eqir (a, b, c) regs = set c (fromEnum (a         == regs !! b)) regs
interp Eqri (a, b, c) regs = set c (fromEnum (regs !! a == b        )) regs
interp Eqrr (a, b, c) regs = set c (fromEnum (regs !! a == regs !! b)) regs


---------- parsing ----------

parseOp :: Parsec String a (Instruction, IData)
parseOp = do
  opCode <- parseOpCode
  space
  a <- nat
  space
  b <- nat
  space
  c <- nat
  return (opCode, (a, b, c))

parseOpCode :: Parsec String a Instruction
parseOpCode = try (string "addr") $> Addr
          <|> try (string "addi") $> Addi
          <|> try (string "mulr") $> Mulr
          <|> try (string "muli") $> Muli
          <|> try (string "banr") $> Banr
          <|> try (string "bani") $> Bani
          <|> try (string "borr") $> Borr
          <|> try (string "bori") $> Bori
          <|> try (string "setr") $> Setr
          <|> try (string "seti") $> Seti
          <|> try (string "gtir") $> Gtir
          <|> try (string "gtri") $> Gtri
          <|> try (string "gtrr") $> Gtrr
          <|> try (string "eqir") $> Eqir
          <|> try (string "eqri") $> Eqri
          <|> try (string "eqrr") $> Eqrr

parseIP :: Parsec String a Int
parseIP = do
  string "#ip"
  spaces
  nat

parseInput :: Parsec String a (Int, Seq (Instruction, IData))
parseInput = do
  ip <- parseIP
  spaces
  program <- many1 $ parseOp <* newline
  spaces
  eof
  return (ip, Seq.fromList program)
