module D10 where

import D (Direction (..), dirV, enlist, mmap, r, readInt)

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
    f e ((x, y), v) =
      -- uniq aparently, 0 tied to 9, fk
      sum
        [ n UP,
          n RIGHT,
          n LEFT,
          n DOWN
        ]
      where
        n d = let (dx, dy) = dirV d in f (v + 1) (id (x + dx, y + dy))

p2 i = i

p i = r p1 p2 k
  where
    k = mmap (readInt . enlist) $ lines i
