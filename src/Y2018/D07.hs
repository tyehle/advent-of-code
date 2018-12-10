module Y2018.D07 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.PSQueue (PSQ)
import qualified Data.PSQueue as PQ
import Data.List (foldl', delete)

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
topoSort db0 = reverse $ go initFringe db0 []
  where
    initFringe = PQ.fromList $ map (\k -> k PQ.:-> k) $ Map.keys $ Map.filter (\(Dep parents _) -> 0 == length parents) db0
    go :: PSQ String String -> Map String Dep -> [String] -> [String]
    go fringe db result = step $ PQ.minView fringe
      where
        step Nothing = result
        step (Just (binding, smallerFringe)) = go newFringe newDB (PQ.key binding : result)
          where
            (addToFringe, newDB) = removeFromDB (PQ.key binding) db
            newFringe = foldl' (\que str -> PQ.insert str str que) smallerFringe addToFringe

    removeFromDB :: String -> Map String Dep -> ([String], Map String Dep)
    removeFromDB key db = foldl' removeParent ([], db) children
      where (Dep _ children) =  db Map.! key
            killParent :: Maybe Dep -> Maybe Dep
            killParent Nothing = error "tried to delete a parent from a nonexistant child"
            killParent (Just (Dep parents children)) = Just (Dep (delete key parents) children)

            removeParent :: ([String], Map String Dep) -> String -> ([String], Map String Dep)
            removeParent (fringe, db) child
              | orphan $ updatedDB Map.! child = (child:fringe, updatedDB)
              | otherwise = (fringe, updatedDB)
              where updatedDB = Map.alter killParent child db
