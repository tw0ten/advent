module D9 where

import D0 (enlist, readInt, rest)

p1 i = ()

p2 i = ()

p i = do
  print $ p1 k
  print $ p2 k
  where
    k = map (readInt . enlist) . reverse . rest $ reverse i
