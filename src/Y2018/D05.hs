module Y2018.D05 where

import Data.Char



-- data Event = FallsAsleep UTCTime | WakesUp UTCTime | StartShift Int UTCTime deriving (Show, Eq)
-- instance Ord Event
--     where compare x y = getTime x `compare` getTime y



run :: IO ()
run = do
  fileName <- getLine
  -- parse the input into a record
  polymer <- init <$> readFile fileName
  let answer = reduce polymer
  print . length $ answer

xor :: Bool -> Bool -> Bool
xor False True = True
xor True False = True
xor _ _ = False

-- [1518-08-08 00:45] falls asleep
reduce :: String -> String
reduce x = let next = reduceRec "" x in if x == next then x else reduce next
    where
        reduceRec :: String -> String -> String
        reduceRec done (x1:x2:xs)
            | (isUpper x1 `xor` isUpper x2) && (toLower x1 == toLower x2) = reduceRec done xs
            | otherwise = reduceRec (x1:done)  (x2:xs)
        reduceRec done x = reverse done ++ x
