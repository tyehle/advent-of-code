module Main where

<<<<<<< HEAD
=======
import System.IO (hFlush, stdout)

>>>>>>> 2ca8513ec8b10ceb5f54941a2ca54950f6fa13f6
import Y2018.D06

main :: IO ()
main = do
  putStr "Input file: "
  hFlush stdout
  fileName <- getLine
  run fileName
