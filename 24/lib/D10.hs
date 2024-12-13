module D10 where

p1 i = sum $ map (sum . map (f 0)) k
  where
    k = [[((x, y), v) | (y, v) <- zip [0 ..] r] | (x, r) <- zip [0 ..] i]
    id (x, y)
      | x < 0 || y < 0 || x >= length k || y >= length (k !! x) = ((x, y), 0)
      | otherwise = k !! x !! y

    f e (_, v)
      | not $ e == v = 0
      | v == t = 1
      where
        t = 9
    f e ((x, y), v) = -- uniq aparently, 0 tied to 9, fk
      sum
        [ n (1, 0),
          n (0, 1),
          n (0, -1),
          n (-1, 0)
        ]
      where
        n (dx, dy) = f (v + 1) (id (x + dx, y + dy))

p2 i = i

p i = do
  print $ p1 k
  print $ p2 k
  where
    k = map (map (\c -> read [c] :: Int)) $ lines i
