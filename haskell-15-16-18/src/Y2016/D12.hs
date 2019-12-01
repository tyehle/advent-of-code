module Y2016.D12 where

import Text.Parsec
import Text.Parsec.String
import Control.Monad (void)
import Data.Array
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)

run :: IO ()
run = do
  instructions <- parseInstructions <$> readFile "resources/d12.txt"
  let program = listArray (0, toInteger (length instructions - 1)) instructions
  putStrLn "with c=0:"
  print $ runProgram program 0 (Map.fromAscList [(r,0) | r <- ['a'..'d']])
  putStrLn "with c=1:"
  print $ runProgram program 0 (Map.fromAscList [('a',0), ('b', 0), ('c', 1), ('d', 0)])

data Instruction = Put Integer Char | Copy Char Char
                 | Inc Char | Dec Char
                 | Jmp Integer | NoOp | JmpC Char Integer
                 deriving (Show)

parseInstructions :: String -> [Instruction]
parseInstructions = either (error.show) id . parse (sepEndBy instructionP newline <* eof) "input"

instructionP :: Parser Instruction
instructionP =  try putP <|> try copyP
            <|> try incP <|> try decP
            <|> try jmpP <|> try jmpcP
  where
    word = try . string
    whitespace = void $ many1 space
    number = do
      neg <- maybe id (const negate) <$> optionMaybe (char '-')
      ds <- read <$> many1 digit
      return $ neg ds
    putP = Put <$> (word "cpy" *> whitespace *> number) <*> (whitespace *> lower)
    copyP = Copy <$> (word "cpy" *> whitespace *> lower) <*> (whitespace *> lower)
    incP = Inc <$> (word "inc" *> whitespace *> lower)
    decP = Dec <$> (word "dec" *> whitespace *> lower)
    jmpP = do
      _ <- word "jnz"
      whitespace
      n <- number
      whitespace
      l <- number
      return $ if n == 0 then NoOp else Jmp l
    jmpcP = JmpC <$> (word "jnz" *> whitespace *> lower) <*> (whitespace *> number)


runProgram :: Array Integer Instruction -> Integer -> Map Char Integer -> Map Char Integer
runProgram prog pos regs | pos > snd (bounds prog) = regs
                         | otherwise = case prog ! pos of
                           Put i r -> step $ insert r i
                           Copy s d -> step $ insert d (get s)
                           Inc r -> step $ insert r (get r + 1)
                           Dec r -> step $ insert r (get r - 1)
                           NoOp -> step regs
                           Jmp n -> runProgram prog (pos + n) regs
                           JmpC r n -> if get r == 0
                                       then step regs
                                       else runProgram prog (pos + n) regs
  where
    step = runProgram prog (pos+1)
    insert r v = Map.insert r v regs
    get r = fromJust $ Map.lookup r regs
