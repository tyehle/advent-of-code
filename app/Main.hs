module Main where

import System.IO (hFlush, stdout)

import Y2018.D13

main :: IO ()
main = do
  putStr "Input: "
  hFlush stdout
  fileName <- getLine
  run fileName
