module Main where

import System.IO (hFlush, stdout)

import Y2018.D07

main :: IO ()
main = do
  putStr "Input file: "
  hFlush stdout
  fileName <- getLine
  run fileName
