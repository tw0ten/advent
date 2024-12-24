module D17 where

import D (enlist, r, readInt, splitElem)
import Data.Bits (Bits (xor))

co _ 7 = undefined
co (a, _, _) 4 = a
co (_, b, _) 5 = b
co (_, _, c) 6 = c
co _ a = a

op (a, b, c) 0 o p = ((a `div` (2 ^ co (a, b, c) o), b, c), p + 2)
op (a, b, c) 1 o p = ((a, xor b o, c), p + 2)
op (a, b, c) 2 o p = ((a, co (a, b, c) o `mod` 8, c), p + 2)
op (0, b, c) 3 o p = ((0, b, c), p + 2)
op (a, b, c) 3 o p = ((0, b, c), p + 1)

p1 (r, i) = op r 0 0 0

p2 i = i

p i = r p1 p2 k
  where
    k =
      let ([a, b, c], [p]) = splitElem "" $ lines i
       in ((r a, r b, r c), o p)
      where
        r = readInt . d
        o = map (readInt . enlist) . filter ((/=) ',') . d
        d = drop 2 . dropWhile ((/=) ':')
