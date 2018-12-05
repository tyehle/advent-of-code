module Y2018.D04 where

import Data.Time hiding (parseTime)
import Data.List.Split
import Data.List (sort)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Data.Foldable (maximumBy)

data Event = FallsAsleep UTCTime | WakesUp UTCTime | StartShift Int UTCTime deriving (Show, Eq)
instance Ord Event
    where compare x y = getTime x `compare` getTime y


type Database = Map.Map Int (Map.Map Int Int)

getTime :: Event -> UTCTime
getTime (FallsAsleep x)  = x
getTime (WakesUp x)      = x
getTime (StartShift _ x) = x


run :: IO ()
run = do
  fileName <- getLine
  -- parse the input into a record
  events <- map parse . lines <$> readFile fileName
  let (StartShift guard _ : sortedEvents) = sort events
      db = sleepTimes sortedEvents Nothing guard Map.empty
  let (who, when) = findSleepiest db
  print $ who * when
  -- print $ take 10 sortedEvents

  where
      parseTime x = parseTimeOrError False defaultTimeLocale  "%Y-%m-%d %H:%M" x :: UTCTime
      -- [1518-11-11 00:04] Guard #2179 begins shift
      parseEvent :: String -> UTCTime -> Event
      parseEvent "falls asleep" = FallsAsleep
      parseEvent "wakes up"     = WakesUp
      parseEvent x              = StartShift guardNumber
        where
            guardNumber = read $ tail (words x !! 1)


      -- [1518-08-08 00:45] falls asleep
      parse :: String -> Event
      parse line = parseEvent event $ parseTime . tail $ time
          where
              [time, event] = splitOn "] " line



getMinute :: UTCTime -> Int
getMinute = undefined

--                             guard number -> (minute -> count)
--               type Database = Map.Map Int (Map.Map Int Int)
sleepTimes :: [Event] -> Maybe Int -> Int -> Database -> Database
sleepTimes [] _ _ x                                     = x
sleepTimes (FallsAsleep         time : xs) _      guard db = sleepTimes xs (Just . getMinute $ time) guard db
sleepTimes (StartShift newGuard _ : xs) fat _ db = sleepTimes xs fat newGuard db
sleepTimes (WakesUp _ : _) Nothing _ _ = error "Tried to wake up before falling asleep"
sleepTimes (WakesUp             time : xs) (Just fat)    guard db = sleepTimes xs Nothing guard $ Map.insert guard newEntry db
    where
        allMinutes = [fat..getMinute time - 1]
        oldEntry = fromMaybe Map.empty $ Map.lookup guard db
        newEntry = foldl addTime oldEntry allMinutes
        addTime oldTimes t = Map.alter increment t oldTimes
        increment Nothing = Just 1
        increment (Just x) = Just $ x + 1

findSleepiest :: Database -> (Int, Int)
findSleepiest db = (sleepiest, bestMinute)
    where
        timeAsleep = Map.map (sum . map snd . Map.toList) db
        (sleepiest, _) = maximumBy (\(_, a) (_, b) -> a `compare` b) $ Map.toList timeAsleep
        (bestMinute, _) = maximumBy (\(_, a) (_, b) -> a `compare` b) $ Map.toList $ (Map.!) db sleepiest
