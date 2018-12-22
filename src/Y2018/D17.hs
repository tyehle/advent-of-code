module Y2018.D17 where


import Data.Functor (($>))
import Data.List (group, sort)
import Data.Maybe (isNothing, catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Parsec
import Text.Parsec.Char


import Interactive
import ParseUtil


type Loc = (Int, Int)


run :: String -> IO ()
run fileName = do
  clay <- unsafeParse parseInput fileName <$> readFile fileName
  let top = minimum . map snd . Set.toList $ clay
      initialState = (Set.empty, Set.empty, [(500,0)])
      finalState@(water, flow, _) = fix (step clay) initialState
      allWater = Set.filter ((>= top) . snd) $ Set.union flow water
  runInteraction (prettyState clay) id (step clay) initialState
  -- putStrLn $ prettyState clay finalState
  print $ Set.size allWater
  print $ Set.size water
  where
    prettyState clay (w, f, _) = prettyWorld clay w f


fix :: Eq a => (a -> a) -> a -> a
fix f x
  | next == x = next
  | otherwise = fix f next
  where
    next = f x



step :: Set Loc -> (Set Loc, Set Loc, [Loc]) -> (Set Loc, Set Loc, [Loc])
step clay (water, flow, fringe) = go (water, flow, []) (map head . group . sort $ fringe)
  where
    bottom = maximum . map snd . Set.toList $ clay
    above, below, left, right :: Loc -> Loc
    above (x, y) = (x, y-1)
    below (x, y) = (x, y+1)
    left  (x, y) = (x-1, y)
    right (x, y) = (x+1, y)

    go :: (Set Loc, Set Loc, [Loc]) -> [Loc] -> (Set Loc, Set Loc, [Loc])
    go state [] = state
    go (water, flow, nextFringe) (f:fs)
      | not $ blocked (below f) = go (water, Set.union flow staticDownFlow, maybe nextFringe (:nextFringe) downFlowEnd) fs
      | isNothing leftFlowEnd && isNothing rightFlowEnd = go (Set.union water pool, Set.difference flow pool, flowsAbovePool ++ nextFringe) fs
      | otherwise = go (water, Set.unions [staticLeftFlow, staticRightFLow, flow], catMaybes [leftFlowEnd, rightFlowEnd] ++ nextFringe) fs
      where
        blocked :: Loc -> Bool
        blocked loc = Set.member loc clay || Set.member loc water

        (staticDownFlow, downFlowEnd) = flowDown f Set.empty

        flowDown :: Loc -> Set Loc -> (Set Loc, Maybe Loc)
        flowDown from seen
          | snd from > bottom = (seen, Nothing)
          | blocked (below from) = (Set.insert from seen, Just from)
          | otherwise = flowDown (below from) $ Set.insert from seen

        (staticLeftFlow, leftFlowEnd) = flowSideways left f Set.empty
        (staticRightFLow, rightFlowEnd) = flowSideways right f Set.empty

        pool = Set.union staticLeftFlow staticRightFLow
        flowsAbovePool = filter (`Set.member` flow) $ above <$> Set.toList pool

        flowSideways :: (Loc -> Loc) -> Loc -> Set Loc -> (Set Loc, Maybe Loc)
        flowSideways direction from seen
          | not $ blocked (below from) = (Set.insert from seen, Just from)
          | blocked (direction from) = (Set.insert from seen, Nothing)
          | otherwise = flowSideways direction (direction from) $ Set.insert from seen


---------- pretty printing ----------

prettyWorld :: Set Loc -> Set Loc -> Set Loc -> String
prettyWorld clay water flow = unlines $ firstRow : go [] [] left 1
  where
    bottom = maximum . map snd . Set.toList $ clay
    left   = subtract 1 . minimum . map fst . Set.toList $ clay
    right  = (+1) . maximum . map fst . Set.toList $ clay

    groundGlyph = "\ESC[90m.\ESC[0m"
    springGlyph = "\ESC[36m+\ESC[0m"
    clayGlyph   = "\ESC[90m#\ESC[0m"
    waterGlypy  = "\ESC[1;38;5;4m~\ESC[0m"
    flowGlyph   = "\ESC[1;38;5;6m|\ESC[0m"

    firstRow :: String
    firstRow = concat $ replicate (500 - left) groundGlyph ++ [springGlyph] ++ replicate (right - 500) groundGlyph

    go :: [String] -> [String] -> Int -> Int -> [String]
    go rows row x y
      | y > bottom = reverse rows
      | x > right = go (concat (reverse row) : rows) [] left (y+1)
      | Set.member (x, y) clay  = go rows (clayGlyph   : row) (x+1) y
      | Set.member (x, y) water = go rows (waterGlypy  : row) (x+1) y
      | Set.member (x, y) flow  = go rows (flowGlyph   : row) (x+1) y
      | otherwise               = go rows (groundGlyph : row) (x+1) y


---------- parsing ----------

parseLine :: Bool -> Parsec String a (Set Loc)
parseLine isColumn = do
  string $ first ++ "="
  single <- nat
  string $ ", " ++ second ++ "="
  start <- nat
  string ".."
  stop <- nat
  return . Set.fromList $ map (if isColumn then id else swap) [(single, run) | run <- [start..stop]]
  where
    (first, second) = if isColumn then ("x", "y") else ("y", "x")
    swap (a, b) = (b, a)

parseInput :: Parsec String a (Set Loc)
parseInput = go Set.empty
  where
    go clay = eof $> clay
          <|> (Set.union clay <$> try (parseLine True <* spaces) >>= go)
          <|> (Set.union clay <$> (parseLine False <* spaces) >>= go)
