{-# LANGUAGE TupleSections #-}
module D13 where

import Data.Maybe
import Data.List
import Data.Ord

parse :: String -> (Integer, [Maybe Integer])
parse input = (read eta, map readTime $ splitOn (== ',') times)
  where
    [eta, times] = lines input
    readTime "x" = Nothing
    readTime t = Just $ read t

splitOn :: (Char -> Bool) -> String -> [String]
splitOn p s = case dropWhile p s of
  "" -> []
  s' -> let (w, s'') = break p s' in w : splitOn p s''

waitTime eta bus = (bus - (eta `mod` bus)) `mod` bus

part1 (eta, times) = bestBus * waitTime eta bestBus
  where
    bestBus = minimumBy (comparing (waitTime eta)) (catMaybes times)

part2 :: (Integer, [Maybe Integer]) -> Integer
part2 (_, buses) = crt pairs
  where
    pairs = map (\(i, bus) -> ((bus - i) `mod` bus, bus)) $ mapMaybe (\(i, bus) -> fmap (i,) bus) $ zip [0..] buses

extendedEuclid :: Integer -> Integer -> (Integer, Integer, Integer)
extendedEuclid a b = go (a, 1, 0) (b, 0, 1)
  where
    go (rp, sp, tp) (r, s, t) = if rn == 0 then (r, s, t) else go (r, s, t) (rn, sn, tn)
      where
        q = rp `div` r
        rn = rp - q*r
        sn = sp - q*s
        tn = tp - q*t

crt :: [(Integer, Integer)] -> Integer
crt pairs = sum (zipWith (*) as $ zipWith (*) bigNs bigMs) `mod` prodN
  where
    as = fst <$> pairs
    ns = snd <$> pairs
    prodN = product ns
    bigNs = (prodN `div`) <$> ns
    bigMs = (\(r, s, t) -> s) <$> zipWith extendedEuclid bigNs ns

run :: IO ()
run = do
  input <- parse <$> readFile "input/13"
  print $ part1 input
  print $ part2 input
