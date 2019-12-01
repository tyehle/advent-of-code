module Y2018.D20 where

import Control.Monad.State.Strict
import Data.Functor (($>))
import Data.Foldable (foldl')
import Data.List (sort, group)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Parsec hiding (State)
import Text.Parsec.Char

import ParseUtil
import Search

data Direction = N | E | S | W deriving (Eq, Ord, Show)

data Path = Atom Direction
          | Concat [Path]
          | Union [Path]
          deriving (Eq, Ord, Show)

type Loc = (Int, Int)


run :: String -> IO ()
run fileName = do
  path <- unsafeParse parseInput fileName <$> readFile fileName
  let world = buildWorld path
  print . subtract 1 . length $ longestWalk world (0, 0)
  let farAway = longPaths world (0, 0)
  print $ longPaths world (0, 0)


longPaths :: Map Loc [Loc] -> Loc -> Int
longPaths world from = execState (bfsM step finishPaths from) 0
  where
    step loc = Map.findWithDefault [] loc world
    finishPaths :: [[Loc]] -> State Int ()
    finishPaths paths = modify' (+ count)
      where
        count = length . dedup . map head . filter ((> 1000) . length) $ paths
        dedup = map head . group . sort


longestWalk :: Map Loc [Loc] -> Loc -> [Loc]
longestWalk world from = execState (bfsM step finishPaths from) [from]
  where
    step :: Loc -> [Loc]
    step loc = Map.findWithDefault [] loc world

    finishPaths :: [[Loc]] -> State [Loc] ()
    finishPaths paths = put (head paths)


buildWorld :: Path -> Map Loc [Loc]
buildWorld path = execState (walkPath (Set.singleton (0,0)) path) Map.empty


walkPath :: Set Loc -> Path -> State (Map Loc [Loc]) (Set Loc)
walkPath from (Atom d) = Set.fromList <$> mapM walkDirection (Set.toList from)
  where walkDirection f = addConnection f dest $> dest
          where dest = move d f
walkPath from (Concat paths) = foldM walkPath from paths
walkPath from (Union paths) = Set.unions <$> mapM (walkPath from) paths


addConnection :: Loc -> Loc -> State (Map Loc [Loc]) ()
addConnection from to = modify $ Map.alter (Just . maybe [to] (to:)) from


move :: Direction -> Loc -> Loc
move N (x, y) = (x, y+1)
move E (x, y) = (x+1, y)
move S (x, y) = (x, y-1)
move W (x, y) = (x-1, y)


flatten :: Path -> Path
flatten (Union [p]) = flatten p
flatten (Concat [p]) = flatten p
flatten (Union ps) = Union . map flatten $ ps
flatten (Concat ps) = Concat . map flatten $ ps
flatten path = path


---------- parsing ----------

parseInput :: Parsec String () Path
parseInput = flatten <$> (char '^' *> unionPath <* char '$' <* spaces <* eof)

unionPath :: Parsec String () Path
unionPath = Union <$> sepBy1 catPath (char '|')

catPath :: Parsec String () Path
catPath = Concat <$> many atom

atom :: Parsec String () Path
atom = Atom <$> direction
   <|> between (char '(') (char ')') unionPath

direction :: Parsec String () Direction
direction = char 'N' $> N
        <|> char 'S' $> S
        <|> char 'E' $> E
        <|> char 'W' $> W
