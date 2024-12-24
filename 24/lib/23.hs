module D23 where

import D (count, r, splitElem)

-- map filter with 3 random cs?
p1 i = count f $ map s i
  where
    f ('t' : _, _, _) = True
    f (_, 't' : _, _) = True
    f (_, _, 't' : _) = True
    f _ = False
    s (a, b) = id (a, b, "  ")

p2 i = ()

p i = r p1 p2 k
  where
    k = map (splitElem '-') $ lines i
