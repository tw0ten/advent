module D9 where

import D (dropBack, enlist, r, readInt, rest)

p1 i = i

p2 i = ()

p i = r p1 p2 k
  where
    k = map (readInt . enlist) $ dropBack 1 i
