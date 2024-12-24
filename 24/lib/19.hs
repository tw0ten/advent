module D19 where

import D (count, parse, r, splitElem)
import Debug.Trace

p1 (i, p) = count id $ map s p
  where
    s "" = True
    s r = or $ map f i
      where
        f c = parse r c && s (drop (length c) r)

p2 i = ()

p i = r p1 p2 k
  where
    k =
      let ([g], p) = splitElem "" $ lines i
       in (map (takeWhile ((/=) ',')) . words $ g ++ ",", p)
