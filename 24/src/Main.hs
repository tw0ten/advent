module Main where

import D1
import D2
import D3
import D4
import D5
import D6
import D7
import D8
import D9
import D10
import D11
import D12
import D13
import D14
import D15
import D16
import D17
import D18
import D19
import D20
import D21
import D22
import D23
import D24
import D25

main :: IO ()
main = do
  i
    1
    [ D1.p,
      D2.p,
      D3.p,
      D4.p,
      D5.p,
      D6.p,
      (\_ -> return ()),
      D8.p,
      D9.p,
      D10.p,
      (\_ -> return ()),
      D12.p,
      D13.p,
      D14.p,
      D15.p,
      D16.p,
      D17.p,
      D18.p,
      D19.p,
      D20.p,
      D21.p,
      D22.p,
      D23.p,
      D24.p,
      D25.p
    ]
  where
    i x (f : r) = do
      putStrLn $ "/" ++ z
      y <- readFile $ "i/" ++ z
      _ <- f y
      i (x + 1) r
      where
        z = show x
    i _ _ = return ()
