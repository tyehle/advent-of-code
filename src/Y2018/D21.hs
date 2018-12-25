{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Y2018.D21 where


import Data.Bits ((.&.), (.|.))
import Data.Functor (($>))
import Data.List (foldl', (\\))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
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
  print $ lammaStar ip program
  print $ goatStar ip program


lammaStar :: Int -> Seq (Instruction, IData) -> Int
lammaStar ipReg instructions = go (0, [0, 0, 0, 0, 0, 0]) !! checkReg
  where
    (checkIndex, checkReg) = findCheck instructions

    go :: (Int, [Int]) -> [Int]
    go (ip, regs)
      | ip == checkIndex = regs -- breakpoint
      | otherwise = case Seq.lookup ip instructions of
        Nothing -> regs
        Just instruction -> go $ interpIP ipReg instruction (ip, regs)


goatStar :: Int -> Seq (Instruction, IData) -> Int
goatStar ipReg instructions = go Set.empty 0 (0, [0, 0, 0, 0, 0, 0])
  where
    (index, reg) = findCheck instructions

    go :: Set Int -> Int -> (Int, [Int]) -> Int
    go !seen !latest (!ip, !regs)
      | ip == index && Set.member (regs !! reg) seen = latest
      | otherwise = case Seq.lookup ip instructions of
        Nothing -> latest
        Just instruction -> go nextSeen nextLatest (interpIP ipReg instruction (ip, regs))
      where
        nextSeen = if ip == index then Set.insert (regs !! reg) seen else seen
        nextLatest = if ip == index then regs !! reg else latest


findCheck :: Seq (Instruction, IData) -> (Int, Int)
findCheck instructions = (index, reg)
  where
    matches (i, (a, b, _)) = i == Eqrr && (a == 0 || b == 0)
    index = fromMaybe (error "Check instruction not found") $ Seq.findIndexL matches instructions
    reg = head . filter (/= 0) . (\(a,b,_) -> [a,b]) . snd . Seq.index instructions $ index


interpIP :: Int -> (Instruction, IData) -> (Int, [Int]) -> (Int, [Int])
interpIP ipReg (i, d) (ip, regs) = (newIP, postRegs)
  where
    preRegs = set ipReg ip regs
    postRegs = interp i d preRegs
    newIP = (postRegs !! ipReg) + 1


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
