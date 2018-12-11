module Y2018.D08 where

import Text.Parsec
import Text.Parsec.Char
import Control.Monad (replicateM)

data Corn = Corn [Corn] [Int]

run :: String -> IO ()
run fileName = do
  -- parent, child pairs
  stalk <- parse corn fileName <$> readFile fileName
  let stalker = either (error . show) id stalk
  print $ sumMetaCorn stalker
  print $ smuttyStar stalker


smuttyStar :: Corn -> Int
smuttyStar (Corn []       metaCorn) = sum metaCorn
smuttyStar (Corn children metaCorn) = sum $ safeAccess <$> metaCorn
  where safeAccess :: Int -> Int
        safeAccess idx
          | (idx > 0) && (idx <= length children ) = smuttyStar (children !! (idx - 1))
          | otherwise = 0

sumMetaCorn :: Corn -> Int
sumMetaCorn (Corn children metaCorn) = sum metaCorn + sum (sumMetaCorn <$> children)

number :: Parsec String () Int
number = read <$> many1 digit

corn :: Parsec String () Corn
corn = do
  numChildren <- number
  space
  numMeta <- number
  childrenOfTheCorn <- replicateM numChildren $ space >> corn
  metaCorn <- replicateM numMeta $ space >> number
  return $ Corn childrenOfTheCorn metaCorn
