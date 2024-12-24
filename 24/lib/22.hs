module D22 where

import D (loop, r, readInt)

p1 i = loop 2000 (\_ -> [0]) i

p2 i = ()

p i = r p1 p2 k
  where
    k = map readInt $ lines i
