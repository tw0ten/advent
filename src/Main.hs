module Main where

import D1
import D2

main :: IO ()
main = do
  putStrLn "D1"
  D1.main
  putStrLn "D2"
  D2.main
