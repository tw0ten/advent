module D11 where

f 0 = [1]
f n
  | even x = [n1, n2]
  where
    x = l n
      where
        l 0 = 0
        l n = 1 + l (n `div` 10)
    n0 = 10 ^ (x `div` 2)
    n1 = n `div` n0
    n2 = n - (n1 * n0)
f n = [n * 2024]

loop 0 _ i = i
loop x e i = loop (x - 1) e (e i)

p1 i = length $ loop 25 (r . map f) i
  where
    r (x : xs) = x ++ r xs
    r _ = []

count y (x : xs) = y x + count y xs
count _ _ = 0

p2 i = sum . map (\(c, _) -> c) . loop 75 (c . r . map w) $ map (\c -> (1, c)) i
  where
    r (x : xs) = x ++ r xs
    r _ = []

    c ((a, n) : xs) = (a + count (\(a, c) -> if c == n then a else 0) xs, n) : c (filter (\(_, c) -> not (c == n)) $ xs)
    c _ = []

    w (a, n) = map (\c -> (a, c)) $ f n

p i = do
  print $ p1 k
  print $ p2 k
  where
    k = map (\i -> read i :: Int) $ words i
