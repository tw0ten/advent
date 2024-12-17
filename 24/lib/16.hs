module D16 where

import D (r)

p1 i = i

p2 i = ()

p i = r p1 p2 k
  where
    k = lines i
