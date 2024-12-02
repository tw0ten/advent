module D where

input = do
  input <- readFile "i/"
  undefined

p1 i = do
  undefined

p2 i = do
  undefined

main = do
  i <- input
  putStrLn "p1"
  p1 i
  putStrLn "p2"
  p2 i
