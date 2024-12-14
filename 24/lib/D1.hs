module D1 where

import D0 (diff, readInt)
import Data.List (sort)

p1 (l1, l2) = sum $ zipWith diff (sort l1) (sort l2)

p2 (l1, l2) = sum $ map (\b -> b * c b l2) l1
  where
    c t (n : ns) = c t ns + if t == n then 1 else 0
    c _ _ = 0

p i = do
  print $ p1 k
  print $ p2 k
  where
    k = s . map readInt $ words i
    s l = (f odd, f even)
      where
        f p = [x | (x, i) <- zip l [0 ..], p i]
