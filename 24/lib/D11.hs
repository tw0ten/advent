module D11 where

import D0 (intLen, readInt)

f 0 = [1]
f n
  | even x = [n1, n - (n1 * n0)]
  where
    base = 10
    x = intLen base n
    n0 = base ^ (x `div` 2)
    n1 = n `div` n0
f n = [n * 2024]

l 0 _ i = i
l x e i = l (x - 1) e (e i)

r (x : xs) = x ++ r xs
r _ = []

p1 i = length $ l 25 (r . map f) i

p2 i = sum . map (\(c, _) -> c) . l 75 (c . r . map w) $ map (\c -> (1, c)) i
  where
    c ((a, n) : xs) = (a + count (\(a, c) -> if c == n then a else 0) xs, n) : c (filter (\(_, c) -> not $ c == n) xs)
      where
        count y (x : xs) = y x + count y xs
        count _ _ = 0
    c _ = []

    w (a, n) = map (\c -> (a, c)) $ f n

p i = do
  print $ p1 k
  print $ p2 k
  where
    k = map readInt $ words i
