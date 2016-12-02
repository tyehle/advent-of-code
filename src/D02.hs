module D02 where

data Command = U | D | L | R deriving (Show)

run :: IO ()
run = do
  commands <- fmap parseCommands . readFile $ "resources/d02.txt"
  putStr "Command line lengths: "
  print . map length $ commands
  print . map getKey . execCommands (1,1) $ commands

parseCommands :: String -> [[Command]]
parseCommands = map (map fromChar) . lines
  where
    fromChar 'U' = U
    fromChar 'D' = D
    fromChar 'L' = L
    fromChar 'R' = R
    fromChar bad = error $ "can't parse char" ++ show bad

keypad :: [[Integer]]
keypad = [[1,2,3], [4,5,6], [7,8,9]]

getKey :: (Int, Int) -> Integer
getKey (row, col) = (keypad !! row) !! col

execCommand :: (Int, Int) -> Command -> (Int, Int)
execCommand pos@(row, col) cmd = if inBounds newPos then newPos else pos
  where
    inBounds (r,c) = r >= 0 && r < 3 && c >= 0 && c < 3
    move (r,c) U = (r-1,c)
    move (r,c) D = (r+1,c)
    move (r,c) L = (r,c-1)
    move (r,c) R = (r,c+1)
    newPos = move pos cmd

execCommands :: (Int, Int) -> [[Command]] -> [(Int, Int)]
execCommands _ [] = []
execCommands start@(row, col) (cmds:rest) = rowEnd : execCommands rowEnd rest
  where
    rowEnd = foldl execCommand start cmds
