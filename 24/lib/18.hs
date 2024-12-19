module D18 where

import D (r, readInt, splitElem)

p1 (i, (w, h)) = ()

p2 i = i

p i = r p1 p2 k
  where
    k = (map s $ lines i, (71, 71))
      where
        s i =
          let (x, y) = splitElem ',' i
           in (readInt x, readInt y)
