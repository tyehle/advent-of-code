module Y2018.D24 where


import Control.Arrow ((&&&))
import Data.List (sortBy, delete)
import Data.Map (Map, (!?))
import qualified Data.Map as Map
import Data.Maybe (isNothing, fromMaybe)
import Data.Ord (comparing)
import Text.Parsec

import Debug.Trace

import ParseUtil

data Group = Group { units :: Int
                   , health :: Int
                   , immunities :: [String]
                   , weaknesses :: [String]
                   , attack :: Int
                   , attackType :: String
                   , initiative :: Int
                   }
             deriving (Eq, Ord, Show)


kill :: Group -> Int -> Group
kill g dead = g{units = units g - dead}


run :: String -> IO ()
run fileName = do
  initial@(immune, infect) <- unsafeParse parseInput fileName <$> readFile fileName
  print $ part1 initial


-- 14938 -> too high
part1 :: ([Group], [Group]) -> Int
part1 state@(a,b)
  | null a = sum . map units $ b
  | null b = sum . map units $ a
  | otherwise = part1 $ step state


step :: ([Group], [Group]) -> ([Group], [Group])
step gs = runAttack (selectTargets gs) gs


runAttack :: Map Group Group -> ([Group], [Group]) -> ([Group], [Group])
runAttack targets (a, b) = trace debugString (applyDamages a, applyDamages b)
  where
    debugString = "----------\n" ++
      "Immune System:\n" ++
      unlines (map show (sortBy (flip $ comparing effectivePower) a)) ++
      "\nInfection:\n" ++
      unlines (map show (sortBy (flip $ comparing effectivePower) b)) ++
      "\nTargets:\n" ++
      (unlines . map (\(a,d) -> show (units a) ++ "\t->\t" ++ show (units d)) $ Map.toList targets) ++
      "\nDamage:\n" ++
      (unlines . map (\(g,k) -> show k ++ "\t/\t" ++ show (units g)) $ Map.toList allDamage) ++
      "\n----------"

    applyDamages :: [Group] -> [Group]
    applyDamages = filter ((> 0) . units) . map (\g -> maybe g (kill g) $ allDamage !? g)

    allDamage :: Map Group Int
    allDamage = foldl doDamage Map.empty . sortBy attackOrder $ a ++ b

    doDamage :: Map Group Int -> Group -> Map Group Int
    doDamage damages g = case targets !? g of
      Nothing -> damages
      (Just target) ->
        let remaining = max 0 $ units g - fromMaybe 0 (damages !? g)
        in Map.insert target (unitsKilled remaining g target) damages

    unitsKilled :: Int -> Group -> Group -> Int
    unitsKilled remaining attacker target = min (units target) (damage `div` health target)
      where
        damage = damageDealt (attacker{units=remaining}) target


selectTargets :: ([Group], [Group]) -> Map Group Group
selectTargets (aRand, bRand) = go (go Map.empty a b) b a
  where
    a, b :: [Group]
    a = sortBy selectionOrder aRand
    b = sortBy selectionOrder bRand
    go :: Map Group Group -> [Group] -> [Group] -> Map Group Group
    go selections [] _ = selections
    go selections _ [] = selections
    go selections (a:attackers) defenders = case selectTarget a defenders of
      Nothing -> go selections attackers defenders
      (Just target) -> go (Map.insert a target selections) attackers (delete target defenders)


selectTarget :: Group -> [Group] -> Maybe Group
selectTarget g targets = (\(_,_,_,t) -> t) <$> safeHead orderedTargets
  where
    orderableTarget t = (damageDealt g t, effectivePower t, initiative t, t)
    orderedTargets = filter (\(d,_,_,_) -> d /= 0) . sortBy (flip compare) $ map orderableTarget targets
    safeHead [] = Nothing
    safeHead (x:_) = Just x



damageDealt :: Group -> Group -> Int
damageDealt attacker defender = multiplier * units attacker * attack attacker
  where
    multiplier
      | attackType attacker `elem` immunities defender = 0
      | attackType attacker `elem` weaknesses defender = 2
      | otherwise                                      = 1


effectivePower :: Group -> Int
effectivePower g = units g * attack g


selectionOrder :: Group -> Group -> Ordering
selectionOrder = flip . comparing $ effectivePower &&& initiative


attackOrder :: Group -> Group -> Ordering
attackOrder = flip $ comparing initiative


swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)


---------- parsing ----------

parseInput :: Parsec String () ([Group], [Group])
parseInput = do
  string "Immune System:\n"
  immune <- sepEndBy parseGroup newline
  string "\nInfection:\n"
  infect <- sepEndBy parseGroup newline
  eof
  return (immune, infect)

parseGroup :: Parsec String () Group
parseGroup = do
  u <- nat <* spaces
  string "units each with "
  h <- nat <* spaces
  string "hit points "
  (is, ws) <- parseProps
  string "with an attack that does "
  a <- nat <* spaces
  t <- many1 letter <* spaces
  string "damage at initiative "
  i <- nat
  return Group{ units=u
              , health=h
              , immunities=is
              , weaknesses=ws
              , attack=a
              , attackType=t
              , initiative=i
              }

parseProps :: Parsec String () ([String], [String])
parseProps = between (string "(") (string ") ") (do { ps <- parseProp ([], []); string "; " *> parseProp ps <|> return ps })
         <|> return ([], [])

parseProp :: ([String], [String]) -> Parsec String () ([String], [String])
parseProp (is, ws) = (string "weak to "   >> (\ws' -> (is, ws')) <$> propList)
                 <|> (string "immune to " >> (\is' -> (is', ws)) <$> propList)
  where
    propList = sepBy1 (many1 letter) (string ", ")
