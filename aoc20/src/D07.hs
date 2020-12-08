module D07 where

import Debug.Trace

parse :: String -> [(String, [(Int, String)])]
parse = map (line . words . init) . lines
  where
    line input1 =
      let (name, ("contain":input2)) = desc [] input1
          contents = content [] input2
      in (name, contents)
    desc acc (word:rest)
      | word `elem` ["bag", "bags", "bag,", "bags,"] = (unwords (reverse acc), rest)
      | otherwise = desc (word:acc) rest
    content :: [(Int, String)] -> [String] -> [(Int, String)]
    content acc [] = acc
    content acc ["no", "other", "bags"] = []
    content acc (nStr:input) =
      let (name, rest) = desc [] input
      in content ((read nStr, name):acc) rest

part1 input = subtract 1 $ length $ go ["shiny gold"] []
  where
    go [] done = done
    go (name:rest) done
      | name `elem` done = go rest done
      | otherwise = go (rest ++ parents name) (name : done)
    parents name = [n | (n, cs) <- input, name `elem` map snd cs]

part2 input = bags "shiny gold"
  where
    bags name =
      let contents = maybe (error name) id $ lookup name input
      in sum $ map (\(n, inner) -> n + n * bags inner) contents


run :: IO ()
run = do
  input <- parse <$> readFile "input/07"
  print $ part1 input
  print $ part2 input
