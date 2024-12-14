module D9 where

import D (enlist, r, readInt, rest)

p1 i = ()

p2 i = ()

p i = r p1 p2 k
  where
    k = map (readInt . enlist) . reverse . rest $ reverse i
