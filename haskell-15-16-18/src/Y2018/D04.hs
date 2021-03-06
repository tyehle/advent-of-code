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


run :: String -> IO ()
run fileName = do
  -- parse the input into a record
  events <- map parse . lines <$> readFile fileName
  let (StartShift guard _ : sortedEvents) = sort events
      db = sleepTimes sortedEvents Nothing guard Map.empty
  let (who, when) = findSleepiest db
  let (who2, when2) = findSnoriest db
  putStrLn "Sleepiest guard:"
  print who
  print when
  print $ who * when
  putStrLn "Snoriest guard:"
  print who2
  print when2
  print $ who2 * when2

  -- print $ take 10 sortedEvents


parseTime :: String -> UTCTime
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
getMinute x = div (fromIntegral . diffTimeToPicoseconds . utctDayTime $ x) 60000000000000

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
        timeAsleep :: Map.Map Int Int -- guard id to how long they were asleep
        timeAsleep = Map.map (sum . map snd . Map.toList) db
        (sleepiest, _) = maximumBy (\(_, a) (_, b) -> a `compare` b) $ Map.toList timeAsleep
        (bestMinute, _) = maximumBy (\(_, a) (_, b) -> a `compare` b) $ Map.toList $ (Map.!) db sleepiest


cmpOnSnd :: Ord a => (a, a) -> (a, a) -> Ordering
cmpOnSnd (_, a) (_, b) = a `compare` b

-- 😭
cmpOnSndSnd :: (Int, (Int, Int)) -> (Int, (Int, Int)) -> Ordering
cmpOnSndSnd (_, (_, a)) (_, (_, b)) = a `compare` b

findSnoriest :: Database -> (Int, Int)
findSnoriest db = (fst bestGuard, (fst . snd) bestGuard)
    where
        maxAsleep :: Map.Map Int (Int, Int) -- guard id to (best minute, time asleep at that minute)
        maxAsleep = Map.map (maximumBy cmpOnSnd . Map.toList) db
        bestGuard :: (Int, (Int, Int))
        bestGuard = maximumBy cmpOnSndSnd $ Map.toList maxAsleep

        -- (sleepiest, _) = maximumBy cmpOnSnd $ Map.toList maxAsleep
        -- (bestMinute, _) = maximumBy cmpOnSnd $ Map.toList $ (Map.!) db sleepiest





        --
