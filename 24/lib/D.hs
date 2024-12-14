module D where

r p1 p2 k = (show $ p1 k, show $ p2 k)

loop 0 _ i = i
loop x e i = loop (x - 1) e $ e i

enlist x = [x]

diff n1 n2 = abs $ n1 - n2

parse _ [] = True
parse [] _ = False
parse (x : xs) (y : ys) = x == y && parse xs ys

readInt i = read i :: Int

rest = drop 1

intLen _ 0 = 0
intLen b n = 1 + intLen b (n `div` b)

split d i = let (r1, r2) = span d i in (r1, rest r2)

splitElem d = split (notEq d)

notEq a b = not $ a == b
