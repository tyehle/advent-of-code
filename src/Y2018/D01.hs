module Y2018.D01 where

run :: IO ()
run = do
  fileName <- getLine
  file <- readFile fileName
  print $ collectJuicyStar file


collectJuicyStar :: String -> Int
collectJuicyStar file = sum $ map (read . dropPlus) . lines $ file
  where
    dropPlus xs = if '+' `elem` xs then tail xs else xs
