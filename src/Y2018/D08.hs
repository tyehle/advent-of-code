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
