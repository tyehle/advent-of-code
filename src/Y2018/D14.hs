module Y2018.D14 where

import Data.Foldable (toList)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq


run :: String -> IO ()
run input = do
  putStrLn . concatMap show $ swoleStar input
  print $ swolenStar input


swoleStar :: String -> [Int]
swoleStar targetStr = [Seq.index finalNums i | i <- [target..target+9]]
  where
    target = read targetStr :: Int
    finalNums = bigEnough ((0, 1), Seq.fromList [3, 7])
    bigEnough state@(_, nums)
      | Seq.length nums >= target + 10 = nums
      | otherwise = bigEnough $ step state


swolenStar :: String -> Int
swolenStar target = go ((0, 1), Seq.fromList [3, 7])
  where
    size = length target
    digits :: [Int]
    digits = map (read . pure) target
    matches0, matches1 :: Seq Int -> Bool
    matches0 nums = digits == (Seq.index nums <$> [Seq.length nums - size..Seq.length nums - 1])
    matches1 nums = digits == (Seq.index nums <$> [Seq.length nums - size - 1..Seq.length nums - 2])
    go :: ((Int, Int), Seq Int) -> Int
    go state@(_, nums)
      | Seq.length nums >= size && matches0 nums = Seq.length nums - size
      | Seq.length nums >  size && matches1 nums = Seq.length nums - size - 1
      | otherwise = go $ step state


step :: ((Int, Int), Seq Int) -> ((Int, Int), Seq Int)
step ((a, b), nums) = ((newLoc a, newLoc b), newNums)
  where
    total = Seq.index nums a + Seq.index nums b
    newNums
      | total < 10 = nums |> total
      | otherwise = nums |> 1 |> (total `mod` 10)
    newLoc loc = (Seq.index nums loc + 1 + loc) `mod` Seq.length newNums
