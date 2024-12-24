module D21 where

import D (r)

p1 i = sum $ map (c . s) i
  where
    s = id
    c _ = 0

p2 i = ()

p i = r p1 p2 k
  where
    k = lines i
