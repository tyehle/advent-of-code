module Y2018.D05 where



-- data Event = FallsAsleep UTCTime | WakesUp UTCTime | StartShift Int UTCTime deriving (Show, Eq)
-- instance Ord Event
--     where compare x y = getTime x `compare` getTime y



run :: IO ()
run = do
  fileName <- getLine
  -- parse the input into a record
  events <- map parse . lines <$> readFile fileName
  putStrLn "Units remaining:"



-- [1518-08-08 00:45] falls asleep
parse :: String -> String
parse line = undefined
