module Search where

import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

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
