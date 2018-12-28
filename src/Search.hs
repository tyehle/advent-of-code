{-# LANGUAGE ScopedTypeVariables #-}

module Search where

import Data.Foldable (find, foldl')
import Data.Functor (($>))
import Data.List (sort)
import Data.Maybe (fromMaybe, fromJust, isNothing)
import Data.PSQueue (PSQ)
import qualified Data.PSQueue as PQ
import Data.Set (Set)
import qualified Data.Set as Set


bfsM :: forall m a. (Ord a, Applicative m) => (a -> [a]) -> ([[a]] -> m ()) -> a -> m ()
bfsM step finishPaths initial = go Set.empty [[initial]]
  where
      validStep :: Set a -> a -> [a]
      validStep visited = filter (not . (`Set.member` visited)) . step
      advance :: Set a -> [a] -> [[a]]
      advance visited path = [now:path | now <- validStep visited  (head path)]
      go :: Set a -> [[a]] -> m ()
      go _ [] = pure ()
      go visited fringe = finishPaths fringe *> go visited' fringe'
        where
          (visited', fringe') = foldl' explorePath (visited, []) ((map reverse . sort . map reverse) fringe)
          explorePath :: (Set a, [[a]]) -> [a] -> (Set a, [[a]])
          explorePath (visited', fringe') path = let
            newPaths = advance visited' path
            in (visited' `Set.union` Set.fromList (map head newPaths), newPaths ++ fringe')


bfs :: Ord a => (a -> [a]) -> (a -> Bool) -> a -> [[a]]
bfs step done initial = either id (const []) $ bfsM step finishPaths initial
  where
    finishPaths paths
      | not (null finishedPaths) = Left finishedPaths
      | otherwise = Right ()
      where
        finishedPaths = filter (done . head) paths


breadthFirst :: Ord a => (a -> [a]) -> (a -> Bool) -> a -> [a]
breadthFirst step done initial = runBFS step done Set.empty [[initial]]

runBFS :: Ord a => (a -> [a]) -> (a -> Bool) -> Set a -> [[a]] -> [a]
runBFS step done visited fringe = fromMaybe recur $ find (done . head) fringe
  where
    validStep = filter (not . (`Set.member` visited)) . step
    advance history = [now:history | now <- validStep (head history)]
    visited' = visited `Set.union` Set.fromList (map head fringe)
    fringe' = concatMap advance fringe
    recur = runBFS step done visited' fringe'

aStar :: (Ord a) => (a -> [a]) -> (a -> Int) -> (a -> Bool) -> a -> Maybe [a]
aStar step close done initial = reverse <$> runAStar step close done Set.empty queue
  where
    queue = PQ.singleton [initial] (close initial)

runAStar :: forall a. (Ord a) => (a -> [a]) -> (a -> Int) -> (a -> Bool) -> Set a -> PSQ [a] Int -> Maybe [a]
runAStar step close done visited fringe
  | isNothing $ PQ.findMin fringe = Nothing
  | Set.member (head smallest) visited = runAStar step close done visited (PQ.deleteMin fringe)
  | (done . head) smallest = Just smallest
  | otherwise = runAStar step close done visited' fringe'
  where
    smallest :: [a]
    smallest = PQ.key . fromJust . PQ.findMin $ fringe

    validStep :: a -> [a]
    validStep = filter (not . (`Set.member` visited)) . step

    visited' :: Set a
    visited' = Set.insert (head smallest) visited

    advance :: [a] -> [[a]]
    advance history = [s:history | s <- validStep (head history)]

    newBindings = map (\path -> (path, length path + close (head path))) (advance smallest)

    fringe' = foldr (uncurry PQ.insert) (PQ.deleteMin fringe) newBindings
