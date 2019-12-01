module Y2015.D07 where

import Data.List (partition)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Parsec
import Data.Word (Word16)
import Data.Bits ((.&.), (.|.), complement, shiftL, shiftR)

import ParseUtil (unsafeParse)

data BinCode = And | Or | LShift | RShift deriving (Show)
data UnCode = Assign | Not deriving (Show)

data Op = BinOp Lit BinCode Lit String
        | UnOp UnCode Lit String
        deriving (Show)

data Lit = Wire String | Nat Word16 deriving (Show)

run :: String -> IO ()
run fileName = do
  input <- readFile fileName
  let ops = unsafeParse parseInput fileName input
  print $ powerfulStar ops "a"
  print $ energyStar ops "a" "b"

----------------- this is SERIOUS shit right here stop giggling ----------------

getAssignee :: Op -> String
getAssignee (BinOp _ _ _ name) = name
getAssignee (UnOp _ _ name) = name

energyStar :: [Op] -> String -> String -> Word16
energyStar ops whichWire overrideWire = powerfulStar secondOps whichWire
  where
    firstValue = powerfulStar ops whichWire
    secondOps = UnOp Assign (Nat firstValue) overrideWire : filter ((/= overrideWire) . getAssignee) ops

powerfulStar :: [Op] -> String -> Word16
powerfulStar ops whichWire = results Map.! whichWire
  where
    opMap :: Map String Op
    opMap = Map.fromList $ zip (map getAssignee ops) ops

    results :: Map String Word16
    results = Map.fromSet compute (Map.keysSet opMap)

    compute :: String -> Word16
    compute name = case opMap Map.! name of
      (UnOp Assign lit _) -> litVal lit
      (UnOp Not lit _) -> complement (litVal lit)
      (BinOp a And b _) -> litVal a .&. litVal b
      (BinOp a Or b _) -> litVal a .|. litVal b
      (BinOp a LShift b _) -> litVal a `shiftL` fromIntegral (litVal b)
      (BinOp a RShift b _) -> litVal a `shiftR` fromIntegral (litVal b)

    litVal :: Lit -> Word16
    litVal (Wire name) = results Map.! name
    litVal (Nat i) = i

------------------- its a PARSING PARTY oh yeah -------------------------------


parseInput :: Parsec String () [Op]
parseInput = many1 (parseOp <* endOfLine) <* eof

parseOp :: Parsec String () Op
parseOp = try (string "NOT " >> (UnOp Not <$> parseLit <*> parseAssign))
      <|> try (UnOp Assign <$> parseLit <*> parseAssign)
      <|> try (BinOp <$> parseLit <*> parseBinCode <*> parseLit <*> parseAssign)

parseBinCode :: Parsec String () BinCode
parseBinCode = try (string " AND " >> return And)
           <|> try (string " OR " >> return Or)
           <|> try (string " LSHIFT " >> return LShift)
           <|> try (string " RSHIFT " >> return RShift)

parseAssign :: Parsec String () String
parseAssign = string " -> " >> many1 letter

parseLit :: Parsec String () Lit
parseLit = try (Wire <$> many1 letter)
        <|> (Nat . read) <$> many1 digit
