module D11 where

f x i = length $ loop x (r . map f) i
  where
    loop x e i = if x == 0 then i else loop (x - 1) e (e i)
    r ((d, m) : xs) = case m of
      Just a -> [d, a] ++ r xs
      _ -> d : r xs
    r _ = []
    f 0 = (1, Nothing)
    f n
      | even x = (n1, Just n2)
      where
        x = l n
          where
            l 0 = 0
            l n = 1 + l (n `div` 10)
        n1 = n `div` (10 ^ (x `div` 2))
        n2 = n - (n1 * 10 ^ (x `div` 2))
    f n = (n * 2024, Nothing)

p1 = f 25

p2 = f 75

p i = do
  print $ p1 k
  print $ p2 k
  where
    k = map (\i -> read i :: Int) $ words i
