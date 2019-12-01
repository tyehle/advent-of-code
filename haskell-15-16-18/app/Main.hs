module Main where

import System.IO (hFlush, stdout)

import Y2015.D07

main :: IO ()
main = do
  putStr "Input: "
  hFlush stdout
  fileName <- getLine
  putStrLn ""
  run fileName
