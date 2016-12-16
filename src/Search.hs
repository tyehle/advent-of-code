module Search where

import Data.Foldable (find)
import Data.Maybe (fromMaybe, fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.PSQueue (PSQ)
import qualified Data.PSQueue as PQ

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

aStar :: Ord a => (a -> [a]) -> (a -> Int) -> (a -> Bool) -> a -> [a]
aStar step close done initial = runAStar step close done Set.empty queue
  where
    queue = PQ.singleton [initial] (close initial)

runAStar :: Ord a => (a -> [a]) -> (a -> Int) -> (a -> Bool) -> Set a -> PSQ [a] Int -> [a]
runAStar step close done visited fringe = if (done . head) smallest
                                          then smallest
                                          else recur
  where
    smallest = PQ.key . fromJust . PQ.findMin $ fringe
    validStep = filter (not . (`Set.member` visited)) . step
    visited' = Set.insert (head smallest) visited
    advance history = [s:history | s <- validStep (head history)]
    newBindings = map (\path -> (path, (close . head) path)) (advance smallest)
    fringe' = foldr (uncurry PQ.insert) (PQ.deleteMin fringe) newBindings
    recur = runAStar step close done visited' fringe'
