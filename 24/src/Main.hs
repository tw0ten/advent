module Main where

import D0
import D1
import D2
import D3
import D4
import D5

main = do
  i 1 [D1.p, D2.p, D3.p, D4.p, D5.p, D0.p]
  where
    i _ [_] = return ()
    i x (f : r) = do
      putStrLn $ "/" ++ z
      y <- readFile $ "i/" ++ z
      f y
      i (x + 1) r
      where
        z = show x
