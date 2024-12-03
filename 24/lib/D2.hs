module D2 where

v op (p : c : r) = op (p, c) && diff p c <= 3 && v op (c : r)
  where
    diff n1 n2 = abs (n1 - n2)
v _ _ = True

f1 = map (\z -> v (\(x, y) -> x < y) z || v (\(x, y) -> x > y) z)

f2 = length . filter id

p1 i = f2 $ f1 i

p2 i = f2 $ map (or . f1 . r) i
  where
    r n = [take i n ++ drop (i + 1) n | i <- [0 .. length n - 1]]

p i = do
  let k = map (\x -> map (\x -> read x :: Int) x) . map words $ lines i
  print $ p1 k
  print $ p2 k
