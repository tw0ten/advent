module Main where

import D
import D1
import D2
import D3
import D4

main = do
  i 1 [D1.p, D2.p, D3.p, D4.p, D.p]
  where
    i _ [_] = return ()
    i x (f : r) = do
      putStrLn $ " D" ++ z
      y <- readFile $ "i/" ++ z
      f y
      i (x + 1) r
      where
        z = show x
