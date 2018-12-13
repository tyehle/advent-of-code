module Y2018.D11 where


import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict


type Loc = (Int, Int)


run :: String -> IO ()
run input = do
  let sn :: Int
      sn = read input
      powerLevels = mkPowerLevels sn
  print $ squareStar powerLevels 3
  print $ evalState (maxPower (powerLevels Map.!)) Map.empty


maxPower :: (Loc -> Int) -> State (Map (Loc, Int) Int) (Int, (Loc, Int))
maxPower value = do
  let ((loc, n):rest) = allBoxes
  p <- powerOf value loc n
  go (loc, n) p rest
  where
    allBoxes :: [(Loc, Int)]
    allBoxes = [((x, y), n)
               | n <- [1..300]
               , x <- [1..300-n+1]
               , y <- [1..300-n+1]
               ]
    go :: (Loc, Int) -> Int -> [(Loc, Int)] -> State (Map (Loc, Int) Int) (Int, (Loc, Int))
    go best bestPower [] = return (bestPower, best)
    go best bestPower ((loc, n):xs) = do
      power <- powerOf value loc n
      if power > bestPower
        then go (loc, n) power xs
        else go best bestPower xs


powerOf :: (Loc -> Int) -> Loc -> Int -> State (Map (Loc, Int) Int) Int
powerOf _ _ 0 = return 0
powerOf value loc 1 = return $ value loc
powerOf value (x, y) n = do
  maybeAnswer <- gets $ Map.lookup ((x, y), n)
  case maybeAnswer of
    Just answer -> return answer
    Nothing -> do
      answer <- compute
      modify' $ Map.insert ((x, y), n) answer
      return answer
  where
    compute :: State (Map (Loc, Int) Int) Int
    compute = do
      topRight <- powerOf value (x, y) (n-1)
      bottomLeft <- powerOf value (x+1, y+1) (n-1)
      center <- powerOf value (x+1, y+1) (n-2)
      let topLeft = value (x+n-1, y)
          bottomRight = value (x, y+n-1)
      return (topRight + topLeft + bottomRight + bottomLeft - center)


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


squareStar :: Map Loc Int -> Int -> (Int, Loc)
squareStar powerLevels size = maximum [(totalPower (x, y), (x, y)) | x <- [1..300-size+1], y <- [1..300-size+1]]
  where
    totalPower :: Loc -> Int
    totalPower (x, y) = sum $ map (powerLevels Map.!) [(x', y') | x' <- [x..x+size-1], y' <- [y..y+size-1]]
