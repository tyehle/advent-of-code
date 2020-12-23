module D14 where

import Data.Bits
import Data.List
import qualified Data.Map as Map
import Numeric

import Debug.Trace

data MemTree
  = Branch MemTree MemTree
  | Trunk MemTree
  | Leaf Integer
  deriving (Eq, Ord, Show)

data Command
  = SetMask String
  | SetMem Integer Integer
  deriving (Eq, Ord, Show)

parse :: String -> [Command]
parse = map parseLine . lines
  where
    parseLine line
      | "mask = " `isPrefixOf` line = parseMask line
      | otherwise = parseSet line
    parseMask line = SetMask $ drop 7 line
    parseSet line = SetMem loc value
      where
        loc = read $ takeWhile (/= ']') $ drop 4 line
        value = read $ drop 2 $ dropWhile (/= '=') line

applyMask :: String -> Integer -> Integer
applyMask digits i = i .&. and .|. or
  where
    and = readBin $ map (\c -> if c == 'X' then '1' else c) digits
    or = readBin $ map (\c -> if c == 'X' then '0' else c) digits
    readBin :: String -> Integer
    readBin = fst . head . readInt 2 (`elem` "01") (\c -> if c == '0' then 0 else 1)

part1 = go "" Map.empty
  where
    go mask mem [] = sum $ Map.elems mem
    go mask mem (SetMask newMask : rest) = go newMask mem rest
    go mask mem (SetMem loc value : rest) = go mask (Map.insert loc (applyMask mask value) mem) rest

setMemTree :: String -> Integer -> Integer -> MemTree -> MemTree
setMemTree mask loc value = go (reverse mask) loc
  where
    go [] _ _ = Leaf value
    go (mask:rest) loc mem = case (mask, mem) of
      ('X', Trunk both) -> Trunk $ recur both
      ('X', Branch zero one) -> Branch (recur zero) (recur one)
      ('1', Trunk both) -> Branch both (recur both)
      ('1', Branch zero one) -> Branch zero (recur one)
      ('0', Trunk both) -> if thisLoc == 0 then Branch (recur both) both else Branch both (recur both)
      ('0', Branch zero one) -> if thisLoc == 0 then Branch (recur zero) one else Branch zero (recur one)
      where
        thisLoc = loc `mod` 2
        recur = go rest (loc `div` 2)

part2 = go "" initialMem
  where
    initialMem = foldr (\() tree -> Trunk tree) (Leaf 0) (replicate 36 ())
    go mask mem [] = total mem
    go mask mem (SetMask newMask : rest) = go newMask mem rest
    go mask mem (SetMem loc value : rest) = go mask (setMemTree mask loc value mem) rest
    total (Branch zero one) = total zero + total one
    total (Trunk both) = total both * 2
    total (Leaf n) = n

run :: IO ()
run = do
  input <- parse <$> readFile "input/14"
  -- mapM_ print test
  print $ part1 input
  print $ part2 input

test = parse "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\n\
             \mem[8] = 11\n\
             \mem[7] = 101\n\
             \mem[8] = 0"

test2 = parse "mask = 000000000000000000000000000000X1001X\n\
              \mem[42] = 100\n\
              \mask = 00000000000000000000000000000000X0XX\n\
              \mem[26] = 1"