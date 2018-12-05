module Y2018.D06 where

run :: String -> IO ()
run fileName = do
  _ <- readFile fileName
  putStrLn "Testing :)"
