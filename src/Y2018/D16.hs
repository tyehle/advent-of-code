{-# LANGUAGE ScopedTypeVariables #-}

module Y2018.D16 where


import Data.Bits ((.&.), (.|.))
import Text.Parsec
import Text.Parsec.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (foldl', (\\))

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


allInstructions :: [Instruction]
allInstructions = [ Addr, Addi
                  , Mulr, Muli
                  , Banr, Bani
                  , Borr, Bori
                  , Setr, Seti
                  , Gtir, Gtri, Gtrr
                  , Eqir, Eqri, Eqrr]


run :: String -> IO ()
run fileName = do
  (samples, program) <- unsafeParse parseInput fileName <$> readFile fileName
  print $ electricStar samples
  let mapping = (\[x] -> x) <$> fix ruleOutSingles (makeInstMap samples)
  print . head . sheepStar mapping $ program
  return undefined


electricStar :: [Sample] -> Int
electricStar = length . filter ((>= 3) . length . whichMatch allInstructions)


sheepStar :: Map Int Instruction -> [(Int, IData)] -> [Int]
sheepStar imap = foldl' step [0, 0, 0, 0]
  where
    step :: [Int] -> (Int, IData) -> [Int]
    step before (code, idata) = interp (imap Map.! code) idata before


ruleOutSingles :: Map Int [Instruction] -> Map Int [Instruction]
ruleOutSingles possible = fmap ruleOut possible
  where
    singles = map (head . snd) . Map.toList . Map.filter ((== 1) . length) $ possible
    ruleOut [x] = [x]
    ruleOut xs = xs \\ singles


fix :: Eq a => (a -> a) -> a -> a
fix f a
  | next == a = a
  | otherwise = fix f next
  where next = f a


whichMatch :: [Instruction] -> Sample -> [Instruction]
whichMatch possible (before, (_, idata), after) = filter ((== after) . actual) possible
  where
    actual instruction = interp instruction idata before


makeInstMap :: [Sample] -> Map Int [Instruction]
makeInstMap = foldl' mapUpdate initMap
  where initMap = Map.fromList [(x, allInstructions) | x <- [0..15]]
        mapUpdate :: Map Int [Instruction] -> Sample -> Map Int [Instruction]
        mapUpdate oldMap sample@(_, (key,_), _) = Map.adjust matchingOps key oldMap
          where matchingOps = flip whichMatch sample


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

parseOp :: Parsec String a (Int, IData)
parseOp = do
  opCode <- nat
  space
  a <- nat
  space
  b <- nat
  space
  c <- nat
  return (opCode, (a, b, c))

parseRegs :: Parsec String a [Int]
parseRegs = between (char '[') (char ']') $ do
  a <- nat
  string ", "
  b <- nat
  string ", "
  c <- nat
  string ", "
  d <- nat
  return [a, b, c, d]

parseSample :: Parsec String a Sample
parseSample = do
  string "Before:" >> spaces
  before <- parseRegs
  newline
  op <- parseOp
  newline
  string "After:" >> spaces
  after <- parseRegs
  return (before, op, after)

parseSamples :: Parsec String a [Sample]
parseSamples = many1 $ parseSample <* newline <* newline

parseInput :: Parsec String a ([Sample], [(Int, IData)])
parseInput = do
  samples <- parseSamples
  spaces
  program <- many1 $ parseOp <* newline
  spaces
  eof
  return (samples, program)
