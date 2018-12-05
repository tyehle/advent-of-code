module Y2018.D06 where


run :: String -> IO ()
run fileName = do
  _ <- readFile fileName
  putStrLn $ saffronStar ""
  putStrLn $ sumacStar ""


saffronStar :: String -> String
saffronStar _ = "Saffron!"


sumacStar :: String -> String
sumacStar _ = "Sumac!"
