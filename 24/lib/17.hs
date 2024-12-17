module D17 where

import D (enlist, r, readInt, splitElem)
import Data.Bits (Bits (xor))

co (a, _, _) 4 = a
co (_, b, _) 5 = b
co (_, _, c) 6 = c
co _ a = a

op (a, b, c) 0 o = (a `div` (2 ^ co (a, b, c) o), b, c)
op (a, b, c) 1 o = (a, xor b o, c)
op (a, b, c) 2 o = (a, co (a, b, c) o `mod` 8, c)
op (0, b, c) 3 o = (0, b, c)
op (a, b, c) 3 o = (0, b, c) -- pointer???? wat

p1 (r, i) = ()

p2 i = i

p i = r p1 p2 k
  where
    k =
      let ([a, b, c], [p]) = splitElem "" $ lines i
       in ((r a, r b, r c), o p)
      where
        r = readInt . d
        o = map (readInt . enlist) . filter (not . (==) ',') . d
        d = drop 2 . dropWhile (not . (==) ':')
