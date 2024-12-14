module D14 where

import D0

p1 i = ()

p2 i = i

p i = do
  print $ p1 k
  print $ p2 k
  where
    k = lines i
