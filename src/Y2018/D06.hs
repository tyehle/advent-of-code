module Y2018.D05 where

import Data.Char
import qualified Data.Set as Set


run :: IO ()
run = do
  fileName <- getLine
  -- parse the input into a record
  polymer <- init <$> readFile fileName
  print "hi"
