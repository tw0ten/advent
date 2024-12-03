module D1 where

import Data.List (sort)

p1 (l1, l2) = sum $ zipWith diff (sort l1) (sort l2)
  where
    diff n1 n2 = abs (n1 - n2)

p2 (l1, l2) = sum $ map (\b -> b * c b l2) l1
  where
    c t (n : ns) = c t ns + if t == n then 1 else 0
    c _ _ = 0

p i = do
  let k = s . map (\b -> read b :: Int) $ words i
  print $ p1 k
  print $ p2 k
  where
    s l = (f odd, f even)
      where
        f p = [x | (x, i) <- zip l [0 ..], p i]
