module Y2018.D07 where


run :: String -> IO ()
run fileName = do
  deps <- lines <$> readFile fileName
  mapM_ putStrLn deps
