module D2 where

import D0 (diff, readInt)

f1 = map (\z -> v (\(x, y) -> x < y) z || v (\(x, y) -> x > y) z)
  where
    v op (p : c : r) = op (p, c) && diff p c <= 3 && v op (c : r)
    v _ _ = True

p1 i = length . filter id $ f1 i

p2 i = length . filter id $ map (or . f1 . r) i
  where
    r n = [take i n ++ drop (i + 1) n | i <- [0 .. length n - 1]]

p i = do
  print $ p1 k
  print $ p2 k
  where
    k = map (map readInt) . map words $ lines i
