module D19 where

import D (count, parse, r, splitElem)

p1 (i, p) = count id $ map s p
  where
    -- compiler's problem
    s "" = True
    s r = or $ map f i
      where
        f c = parse r c && s d
          where
            d = drop (length c) r

p2 i = ()

p i = r p1 p2 k
  where
    k =
      let ([g], p) = splitElem "" $ lines i
       in (map (takeWhile ((/=) ',')) $ words g, p)
