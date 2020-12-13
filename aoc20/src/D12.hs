module D12 where

import CInt


parse :: String -> [(Char, Integer)]
parse = map parseLine . lines
  where parseLine l = (head l, read (tail l))


step1 :: (Char, Integer) -> (C, C) -> (C, C)
step1 (command, n) (pos, dir)
  | command == 'F' = (pos + (dir * fromInteger n), dir)
  | command `elem` "NESW" = (pos + (toVec command * fromInteger n), dir)
  | command `elem` "RL" = (pos, rotate dir (toVec command) n)
  | otherwise = error $ "Unknown command: " ++ show command

part1 = go ((0, 0), toVec 'E')
  where
    go ((x, y), _) [] = abs x + abs y
    go state (i:is) = go (step1 i state) is


step2 :: (Char, Integer) -> (C, C) -> (C, C)
step2 (command, n) (waypoint, pos)
  | command == 'F' = if n == 0 then (waypoint, pos) else step2 ('F', n-1) (waypoint, pos + waypoint)
  | command `elem` "NESW" = (waypoint + (toVec command * fromInteger n), pos)
  | command `elem` "RL" = (rotate waypoint (toVec command) n, pos)
  | otherwise = error $ "Unknown command: " ++ show command

part2 = go ((10, 1), (0, 0))
  where
    go (_, (x, y)) [] = abs x + abs y
    go state (i:is) = go (step2 i state) is


run :: IO ()
run = do
  -- print $ part2 eg
  input <- parse <$> readFile "input/12"
  print $ part1 input
  print $ part2 input

eg = parse "F10\n\
           \N3\n\
           \F7\n\
           \R90\n\
           \F11"