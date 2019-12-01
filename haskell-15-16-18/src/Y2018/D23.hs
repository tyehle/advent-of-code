{-# LANGUAGE BangPatterns #-}

module Y2018.D23 where

import Control.Arrow ((&&&))
import Data.Maybe
import Data.Foldable (maximumBy, minimumBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Numeric.LinearAlgebra.Data as M
import Numeric.LinearAlgebra (inv, (#>))
import Text.Parsec
import Text.Parsec.Char

import ParseUtil

type Pos = (Int, Int, Int)
type Bot = (Pos, Int)


run :: String -> IO ()
run fileName = do
  bots <- unsafeParse parseInput fileName <$> readFile fileName
  print $ part1 bots
  putStrLn "\n----------\n"
  part2 bots


l1 :: Pos -> Pos -> Int
l1 (x1,y1,z1) (x2,y2,z2) = abs (x1-x2) + abs (y1-y2) + abs (z1-z2)


part1 :: [Bot] -> Int
part1 bots = length . filter inRange $ bots
  where
    (boss, range) = maximumBy (comparing snd) bots
    inRange (pos, _) = l1 boss pos <= range


bronKerbosch :: Map Bot (Set Bot) -> [Set Bot]
bronKerbosch edges = runRecursive Set.empty (Map.keysSet edges) Set.empty
  where
    runRecursive :: Set Bot -> Set Bot -> Set Bot -> [Set Bot]
    runRecursive !r !p !x
      | Set.null p && Set.null x = [r]
      | Set.null p = []
      | otherwise = go [] p x . Set.toList . Set.difference p $ edges Map.! pivot
        where
          pivot :: Bot
          pivot = maximumBy (comparing (Set.size . (edges Map.!))) (Set.union p x)

          go :: [Set Bot] -> Set Bot -> Set Bot -> [Bot] -> [Set Bot]
          go found p x [] = found
          go !found !p !x (v:vs) = go (found ++ maximal) (Set.delete v p) (Set.insert v x) vs
            where
              neighbors = edges Map.! v
              r' = Set.insert v r
              p' = Set.intersection neighbors p
              x' = Set.intersection neighbors x
              maximal = runRecursive r' p' x'


part2 :: [Bot] -> IO ()
part2 bots = do
  putStrLn $ "Found " ++ show (length cliques) ++ " cliques"
  print $ Set.size clique
  print finalPlanes
  let vs = findVertex <$> vertices
  print vs
  print . minimum $ map (l1 (0,0,0)) vs
  where
    touching :: Bot -> Bot -> Bool
    touching (p1, r1) (p2, r2) = l1 p1 p2 <= r1 + r2

    edgeMap :: Map Bot (Set Bot)
    edgeMap = Map.fromList $ map (\b -> (b, Set.fromList $ filter (/= b) $ filter (touching b) bots)) bots

    cliques = bronKerbosch edgeMap
    clique = maximumBy (comparing Set.size) cliques

    vertexIn :: Pos -> Bot -> Pos
    vertexIn ( 1, _, _) ((x,y,z), r) = (x+r,y,z)
    vertexIn (-1, _, _) ((x,y,z), r) = (x-r,y,z)

    vertices :: [Pos -> Bool]
    vertices = [ \(x,_,_) -> x == 1
               , \(x,_,_) -> x == -1
               , \(_,y,_) -> y == 1
               , \(_,y,_) -> y == -1
               , \(_,_,z) -> z == 1
               , \(_,_,z) -> z == -1
               ]

    findVertex :: (Pos -> Bool) -> Pos
    findVertex dirFilter = vecPos v
      where
        directions = take 3 . filter dirFilter $ normals
        a' = inv . M.matrix 3 $ concatMap posList directions
        v = a' #> M.fromList (map (\d -> fromIntegral $ d `dot` (finalPlanes Map.! d)) directions)

        posList :: Pos -> [Double]
        posList (x, y, z) = map fromIntegral [x, y, z]

        vecPos :: M.Vector Double -> Pos
        vecPos v = (x, y, z)
          where
            [x, y, z] = round <$> M.toList v

    normals :: [Pos]
    normals = [ ( 1, 1, 1)
              , ( 1, 1,-1)
              , ( 1,-1, 1)
              , ( 1,-1,-1)
              , (-1, 1, 1)
              , (-1, 1,-1)
              , (-1,-1, 1)
              , (-1,-1,-1)
              ]

    minVertex :: Pos -> Pos
    minVertex direction = minimumBy (comparing (`scalarProjection` direction)) $ Set.map (vertexIn direction) clique

    finalPlanes :: Map Pos Pos
    finalPlanes = Map.fromList $ map (\d -> (d, minVertex d)) normals

-- (d0, d1, d2) inverse `mmult` (d0.m0, d1.m1, d2.m2) = top vertex


scalarProjection :: Pos -> Pos -> Double
scalarProjection p onto@(x,y,z) = fromIntegral (p `dot` onto) / normOnto
  where
    normOnto :: Double
    normOnto = sqrt . fromIntegral $ x*x + y*y + z*z


dot :: Pos -> Pos -> Int
dot (x1, y1, z1) (x2, y2, z2) = x1*x2 + y1*y2 + z1*z2



---------- parsing ----------

parseInput :: Parsec String () [Bot]
parseInput = sepEndBy1 parseBot newline <* eof

parseBot :: Parsec String () Bot
parseBot = do
  pos <- string "pos=" *> parsePos
  string ", "
  radius <- string "r=" *> nat
  return (pos, radius)

parsePos :: Parsec String () Pos
parsePos = between (char '<') (char '>') $ do
  x <- int
  char ','
  y <- int
  char ','
  z <- int
  return (x, y, z)
