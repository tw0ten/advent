module D4 (p) where

import D (ib, parse, r, rest)
import Data.List (transpose)

p1 i =
  sum
    [ sm lo i,
      sm (lo . r) i,
      sm lo tp,
      sm (lo . r) tp,
      di (l + 1) ul,
      di (l + 1) $ r ul,
      di (l - 1) ul,
      di (l - 1) $ r ul
    ]
  where
    sm i = sum . map i
    tp = transpose i
    r = reverse
    ul = unlines i
    l = length i
    lo "" = 0
    lo i = c + lo (rest i)
      where
        c = ib $ parse i "XMAS"
    di _ "" = 0
    di a i = parse a i "XMAS" + (di a $ rest i)
      where
        parse _ _ [] = 1
        parse _ [] _ = 0
        parse a (x : xs) (y : ys) =
          if x == y then parse a (drop a xs) ys else 0

p2 k = sum $ map f [((i, j), v) | (i, r) <- zip [0 ..] k, (j, v) <- zip [0 ..] r]
  where
    f (ij, v) = if v == 'A' then fm ij else 0
      where
        fm (i, j) = ib $ (fs (1, 1) || fs (-1, -1)) && (fs (-1, 1) || fs (1, -1))
          where
            fs (x, y) = id (x + i) (y + j) == 'M' && id (i - x) (j - y) == 'S'
              where
                id i j
                  | i < 0 || j < 0 || i >= l || j >= l = '\0'
                  | otherwise = k !! i !! j
                  where
                    l = length k

p i = r p1 p2 k
  where
    k = lines i
