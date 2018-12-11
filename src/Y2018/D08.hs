module Y2018.D08 where

data Node = Node [Node] [Int]


run :: String -> IO ()
run fileName = do
  -- parent, child pairs
  root <- parse <$> readFile fileName
  print $ easyStar root


easyStar :: Node -> Int
easyStar = undefined

parse :: String -> Node
parse xs = undefined
  where raw = map (\x -> read x :: Int) $ words xs
        getChildNode :: [Int] -> Node
        getChildNode inputs = undefined
          where [numChildren, numMeta] = take 2 inputs
                todo = getChildNode (drop 2 inputs)
