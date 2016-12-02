module D02 where

import Data.Char (toUpper)
import Numeric (showHex)

data Command = U | D | L | R deriving (Show)

run :: IO ()
run = do
  commands <- fmap parseCommands . readFile $ "resources/d02.txt"
  putStr "Square code: "
  putStrLn . concatMap (show . getKey squareKeypad) . execCommands squareBounds (1,1) $ commands
  putStr "Diamond code: "
  putStrLn . map toUpper . foldr (showHex . getKey diamondKeypad) "" . execCommands diamondBounds (2,0) $ commands

parseCommands :: String -> [[Command]]
parseCommands = map (map fromChar) . lines
  where
    fromChar 'U' = U
    fromChar 'D' = D
    fromChar 'L' = L
    fromChar 'R' = R
    fromChar bad = error $ "can't parse char" ++ show bad

squareKeypad :: [[Integer]]
squareKeypad = [[1,2,3], [4,5,6], [7,8,9]]

squareBounds :: (Int, Int) -> Bool
squareBounds (row, col) = row >= 0 && row < 3 && col >= 0 && col < 3

diamondKeypad :: [[Integer]]
diamondKeypad = [ [0,   0,   1,   0,   0]
                , [0,   2,   3,   4,   0]
                , [5,   6,   7,   8,   9]
                , [0, 0xA, 0xB, 0xC,   0]
                , [0,   0, 0xD,   0,   0]
                ]

diamondBounds :: (Int, Int) -> Bool
diamondBounds  = (< 3) . distance
  where
    distance (r,c) = abs (r-2) + abs (c-2)

getKey :: [[a]] -> (Int, Int) -> a
getKey pad (row, col) = (pad !! row) !! col

execCommand :: ((Int,Int) -> Bool) -> (Int, Int) -> Command -> (Int, Int)
execCommand inBounds pos@(row, col) cmd = if inBounds newPos then newPos else pos
  where
    move (r,c) U = (r-1,c)
    move (r,c) D = (r+1,c)
    move (r,c) L = (r,c-1)
    move (r,c) R = (r,c+1)
    newPos = move pos cmd

execCommands :: ((Int,Int) -> Bool) -> (Int, Int) -> [[Command]] -> [(Int, Int)]
execCommands _ _ [] = []
execCommands inBounds start@(row, col) (cmds:rest) = rowEnd : execCommands inBounds rowEnd rest
  where
    rowEnd = foldl (execCommand inBounds) start cmds
