module Y2018.D07 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.PSQueue (PSQ)
import qualified Data.PSQueue as PQ
import Data.List (foldl', delete)
import Data.Char (ord)
import Data.Maybe (isNothing)
import Control.Monad.State

-- | Parents and Children of a node
data Dep = Dep [String] [String] deriving (Show)

orphan :: Dep -> Bool
orphan (Dep [] _) = True
orphan _ = False

run :: String -> IO ()
run fileName = do
  -- parent, child pairs
  pairs <- map (getWords . words) . lines <$> readFile fileName
  let deps = foldl' (flip populate) Map.empty pairs
  print $ concat $ topoSort deps
  print $ timelyStar deps - 1

populate ::
  (String, String) -> -- | parent, child pair
  Map String Dep ->
  Map String Dep
populate (parent, child) = Map.alter addChild parent . Map.alter addParent child
  where
    addChild :: Maybe Dep -> Maybe Dep
    addChild Nothing = Just $ Dep [] [child]
    addChild (Just (Dep parents children)) = Just $ Dep parents (child:children)
    addParent :: Maybe Dep -> Maybe Dep
    addParent Nothing = Just $ Dep [parent] []
    addParent (Just (Dep parents children)) = Just $ Dep (parent:parents) children


-- Step S must be finished before step G can begin.
getWords :: [String] -> (String, String)
getWords [_, parent, _, _, _, _, _, child, _, _] = (parent, child)


topoSort :: Map String Dep -> [String]
topoSort db0 = reverse $ go (initFringe db0) db0 []
  where
    go :: PSQ String String -> Map String Dep -> [String] -> [String]
    go fringe db result = case popFringe db fringe of
      Nothing -> result
      Just (what, newDB, newFringe) -> go newFringe newDB (what : result)


popFringe :: Map String Dep -> PSQ String String -> Maybe (String, Map String Dep, PSQ String String)
popFringe db fringe = case PQ.minView fringe of
  Nothing -> Nothing
  Just (binding, smallerFringe) -> Just $ doRemove binding smallerFringe
  where
    doRemove :: PQ.Binding String String -> PSQ String String -> (String, Map String Dep, PSQ String String)
    doRemove binding smallerFringe = (PQ.key binding, newDB, newFringe)
      where
        (newDB, newFringe) = removeFromDB (PQ.key binding) db smallerFringe


removeFromDB :: String -> Map String Dep -> PSQ String String -> (Map String Dep, PSQ String String)
removeFromDB key db fringe = (newDB, newFringe)
  where
    (Dep _ children) =  db Map.! key

    killParent :: Maybe Dep -> Maybe Dep
    killParent Nothing = error "tried to delete a parent from a nonexistant child"
    killParent (Just (Dep parents children)) = Just (Dep (delete key parents) children)

    removeParent :: ([String], Map String Dep) -> String -> ([String], Map String Dep)
    removeParent (fringe, db) child
      | orphan $ updatedDB Map.! child = (child:fringe, updatedDB)
      | otherwise = (fringe, updatedDB)
      where updatedDB = Map.alter killParent child db

    (toAddToFringe, newDB) = foldl' removeParent ([], db) children
    newFringe = foldl' (\q k -> PQ.insert k k q) fringe toAddToFringe


initFringe :: Map String Dep -> PSQ String String
initFringe = PQ.fromList . map (\k -> k PQ.:-> k) . Map.keys . Map.filter (\(Dep parents _) -> 0 == length parents)


timelyStar :: Map String Dep -> Int
timelyStar db = simulateBuild 0 db [] (initFringe db)


simulateBuild :: Int -> Map String Dep -> [(String, Int)] -> PSQ String String -> Int
simulateBuild t db inProgress fringe
  | null inProgress && isNothing (PQ.findMin fringe) = t
  | otherwise = simulateBuild (t+1) fullDB fullProcs fullFringe
  where
    buildTime :: String -> Int
    buildTime [c] = ord c - 4

    updateProcs :: [(String, Int)] -> Map String Dep -> PSQ String String -> ([(String, Int)], Map String Dep, PSQ String String)
    updateProcs procs db fringe = (newProcs, newDB, newFringe)
      where
        tickedProcs = map (\(k, tRemain) -> (k, tRemain - 1)) procs
        doneProcs = filter ((== 0) . snd) tickedProcs
        newProcs = filter ((/= 0) . snd) tickedProcs
        (newDB, newFringe) = foldl' (\(db', fringe') (k, _) -> removeFromDB k db' fringe') (db, fringe) doneProcs

    ensureFullProcs :: [(String, Int)] -> Map String Dep -> PSQ String String -> ([(String, Int)], Map String Dep, PSQ String String)
    ensureFullProcs procs db fringe
      | length procs == 5 = (procs, db, fringe)
      | otherwise = case PQ.minView fringe of
        Nothing -> (procs, db, fringe)
        Just (binding, smallerFringe) -> ensureFullProcs ((PQ.key binding, buildTime (PQ.key binding)):procs) db smallerFringe

    (tickProcs, tickDB, tickFringe) = updateProcs inProgress db fringe
    (fullProcs, fullDB, fullFringe) = ensureFullProcs tickProcs tickDB tickFringe
