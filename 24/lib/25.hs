module D25 (p) where

import D (count, r, splitL)
import Data.List (partition, zip5)

len = 5

p1 (k, l) = count not . map f $ u k l
  where
    f (a, b) = or . map ((<) len . uncurry (+)) $ zip a b
    u l1 l2 = [(x, y) | x <- l1, y <- l2]

p2 i = ()

p i = r p1 p2 k
  where
    k =
      let (k, l) = partition (\c -> ct (c !! 0) == len) . splitL ((/=) "") $ lines i
       in (map c k, map c l)
      where
        ct = count ((==) '#')
        c [_, i1, i2, i3, i4, i5, _] = map f $ zip5 i1 i2 i3 i4 i5 -- wtf?? in my defence why is there a zip5 in data.list
          where
            f (i1, i2, i3, i4, i5) = ct [i1, i2, i3, i4, i5]
