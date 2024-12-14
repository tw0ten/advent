module D0 where

enlist x = [x]

diff n1 n2 = abs $ n1 - n2

parse _ [] = True
parse [] _ = False
parse (x : xs) (y : ys) = x == y && parse xs ys

readInt i = read i :: Int

rest xs = drop 1 xs

intLen _ 0 = 0
intLen b n = 1 + intLen b (n `div` b)

split d i = let (r1, r2) = span d i in (r1, rest r2)
