module D2 where

import D (diff, mmap, r, readInt)

f0 = length . filter id

f1 i = map (\z -> v (uncurry (<)) z || v (uncurry (>)) z) i
  where
    v op (p : c : r) =
      op (p, c) && diff p c <= 3 && v op (c : r)
    v _ _ = True

p1 i = f0 $ f1 i

p2 i = f0 $ map (or . f1 . r) i
  where
    r n = [take i n ++ drop (i + 1) n | i <- [0 .. length n - 1]]

p i = r p1 p2 k
  where
    k = mmap readInt . map words $ lines i
