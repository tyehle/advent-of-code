module Y2018.D22 where

import Control.Monad.State.Strict
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Text.Parsec hiding (State)
import Text.Parsec.Char

import ParseUtil
import Search


type Loc = (Int, Int)

data Tool = Rope | Torch | None deriving (Eq, Ord, Show)
data Area = Rocky | Wet | Narrow deriving (Eq, Ord, Show, Enum)


tools :: [Tool]
tools = [Rope, Torch, None]


run :: String -> IO ()
run fileName = do
  (depth, target) <- unsafeParse parseInput fileName <$> readFile fileName
  let erosionLevels = buildErosionLevel depth target
  print $ riskTarget erosionLevels target
  let path = fromMaybe (error "No path found") $ findPath erosionLevels target
  -- mapM_ (\((t, _), loc) -> print (t, loc, area erosionLevels loc)) . filter (\((_, n), _) -> n == 0) $ path
  print . subtract 1 . length $ path


adjacent :: Loc -> [Loc]
adjacent (x, y) = filter valid [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
  where
    valid (x, y) = x >= 0 && y >= 0


area :: Map Loc Int -> Loc -> Area
area erosionLevels loc = toEnum $ (erosionLevels Map.! loc) `mod` 3


-- 971 -> too low
findPath :: Map Loc Int -> Loc -> Maybe [((Tool, Int), Loc)]
findPath erosionLevels target@(tx, ty) = aStar step bound done ((Torch, 0), (0, 0))
-- findPath erosionLevels target@(tx, ty) = safeHead $ bfs step done ((Torch, 0), (0, 0))
  where
    step :: ((Tool, Int), Loc) -> [((Tool, Int), Loc)]
    step ((tool, n), loc)
      | n > 0 = [((tool, n-1), loc)]
      | otherwise = changeTools ++ move
      where
        changeTools = map (\t -> ((t, 6), loc)) $ filter (\t -> t /= tool && canUse t (lookupArea loc)) tools
        move = map (\l -> ((tool, 0), l)) . filter (canUse tool  .lookupArea) $ adjacent loc

    bound :: ((Tool, Int), Loc) -> Int
    bound ((tool, n), (x, y)) = n + toolPenalty + distance
      where
        toolPenalty = if tool == Torch then 0 else 7
        distance = abs (x-tx) + abs (y-ty)

    done :: ((Tool, Int), Loc) -> Bool
    done ((Torch, 0), loc) = loc == target
    done _ = False

    canUse :: Tool -> Area -> Bool
    canUse tool Rocky = tool /= None
    canUse tool Wet = tool /= Torch
    canUse tool Narrow = tool /= Rope

    lookupArea :: Loc -> Area
    lookupArea = area erosionLevels


riskTarget :: Map Loc Int -> Loc -> Int
riskTarget erosionLevels target@(x, y) = sum [ erosionLevels Map.! (x', y') `mod` 3
                                             | x' <- [0..x]
                                             , y' <- [0..y]
                                             ]


buildErosionLevel :: Int -> Loc -> Map Loc Int
buildErosionLevel depth target@(tx, ty) = execState (mapM_ go ((0,0) : locations)) Map.empty
  where
    locations = [(x, total-x) | x <- [0..total]]
      where total = (tx+ty) * 3

    fromGeoIndex :: Int -> Int
    fromGeoIndex geo = (geo + depth) `mod` 20183

    saveReturn :: Loc -> Int -> State (Map Loc Int) Int
    saveReturn loc answer = do
      modify' $ Map.insert loc answer
      return answer

    go :: Loc -> State (Map Loc Int) Int
    go loc@(x, y)
      | x==0 && y==0  = saveReturn loc $ fromGeoIndex 0
      | y==0          = saveReturn loc . fromGeoIndex $ x * 16807
      | x==0          = saveReturn loc . fromGeoIndex $ y * 48271
      | loc == target = saveReturn loc $ fromGeoIndex 0
      | otherwise     = do
        fromCache <- gets $ Map.lookup loc
        case fromCache of
          (Just erosionLevel) -> return erosionLevel
          Nothing -> do
            e1 <- go (x-1, y)
            e2 <- go (x, y-1)
            saveReturn loc . fromGeoIndex $ e1 * e2


---------- parsing ----------

parseInput :: Parsec String () (Int, Loc)
parseInput = do
  string "depth: "
  depth <- nat
  spaces
  string "target: "
  x <- nat
  string ","
  y <- nat
  spaces
  eof
  return (depth, (x, y))
