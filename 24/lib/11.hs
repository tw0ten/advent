module D11 (p) where

import D (intLen, loop, r, readInt)

f 0 = [1]
f n
  | even x = [n1, n - (n1 * n0)]
  where
    base = 10
    x = intLen base n
    n0 = base ^ (x `div` 2)
    n1 = n `div` n0
f n = [n * 2024]

p1 i = length $ loop 25 (concatMap f) i

p2 i = sum . map fst . loop 75 (c . concatMap w) $ map (\c -> (1, c)) i
  where
    c ((a, n) : xs) =
      (a + count (\(a, c) -> if c == n then a else 0) xs, n)
        : c (filter ((/=) n . snd) xs)
      where
        count y (x : xs) = y x + count y xs
        count _ _ = 0
    c _ = []
    w (a, n) = map (\c -> (a, c)) $ f n

p i = r p1 p2 k
  where
    k = map readInt $ words i
