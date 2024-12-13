module D0 where

rest = drop 1

intLen _ 0 = 0
intLen b n = 1 + intLen b (n `div` b)

split d i = let (r1, r2) = span d i in (r1, rest r2)
