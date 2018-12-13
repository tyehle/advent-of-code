module Y2018.D11 where


import Data.Map (Map)
import qualified Data.Map as Map


type Loc = (Int, Int)


run :: String -> IO ()
run input = do
  let sn :: Int
      sn = read input
      powerLevels = mkPowerLevels sn
  print $ squareStar powerLevels 3
  print $ cubicalStar powerLevels


maxSubSeq :: (Num a, Ord a) => [a] -> [a]
maxSubSeq [] = []
maxSubSeq (first:rest) = reverse $ go (first, [first]) (first, [first]) rest
  where
    go :: (Num a, Ord a) => (a, [a]) -> (a, [a]) -> [a] -> [a]
    go (_, best) _ [] = best
    go best (scoreHere, here) (x:xs) = go nextBest nextHere xs
      where
        nextHere = if x > scoreHere + x
                   then (x, [x])
                   else (x + scoreHere, x:here)
        nextBest = if fst nextHere > fst best
                   then nextHere
                   else best


mkPowerLevels :: Int -> Map Loc Int
mkPowerLevels sn = Map.fromList [((x, y), powerLevel sn (x, y))
                           | x <- [1..300]
                           , y <- [1..300]
                           ]


powerLevel :: Int -> Loc -> Int
powerLevel sn (x, y) = power4 - 5
  where
    rackID = x + 10
    power1 = rackID * y
    power2 = power1 + sn
    power3 = power2 * rackID
    power4 = (power3 `div` 100) `mod` 10


cubicalStar :: Map Loc Int -> ((Int, Loc), Int)
cubicalStar powerLevels = maximum [(squareStar powerLevels size, size) | size <- [1..300]]


squareStar :: Map Loc Int -> Int -> (Int, Loc)
squareStar powerLevels size = maximum [(totalPower (x, y), (x, y)) | x <- [1..300-size+1], y <- [1..300-size+1]]
  where
    totalPower :: Loc -> Int
    totalPower (x, y) = sum $ map (powerLevels Map.!) [(x', y') | x' <- [x..x+size-1], y' <- [y..y+size-1]]
