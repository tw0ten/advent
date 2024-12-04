module D4 where

import Data.List

parse _ [] = True
parse [] _ = False
parse (x : xs) (y : ys) = x == y && parse xs ys

p1 i =
  0
    + (sm lo i)
    + (sm (lo . r) i)
    + (sm lo tp)
    + (sm (lo . r) tp)
    + (di (l + 1) ul)
    + (di (l + 1) $ r ul)
    + (di (l - 1) ul)
    + (di (l - 1) $ r ul)
  where
    sm i = sum . map i
    tp = transpose i
    r = reverse
    ul = unlines i
    l = length i
    lo "" = 0
    lo i = count i + lo (drop 1 i)
      where
        count i = if parse i "XMAS" then 1 else 0
    di _ "" = 0
    di a i = (parse a i "XMAS") + (di a (drop 1 i))
      where
        parse _ _ [] = 1
        parse _ [] _ = 0
        parse a (x : xs) (y : ys) = if x == y then parse a (drop a xs) ys else 0

p2 k =
  sum $
    map
      f
      [ (i, j, v)
      | (i, row) <- zip [0 ..] k,
        (j, v) <- zip [0 ..] row
      ]
  where
    l = length k
    id i j
      | i < 0 || j < 0 || i >= l || j >= l = ' '
      | otherwise = k !! i !! j
    f (i, j, v) = if v == 'A' then fm (i, j) else 0
    fm (i, j) = if (fs (i, j) (1, 1) || fs (i, j) (-1, -1)) && (fs (i, j) (-1, 1) || fs (i, j) (1, -1)) then 1 else 0
    fs (x, y) (i, j) = if id (x + i) (y + j) == 'M' then id (x - i) (y - j) == 'S' else False

p i = do
  let k = lines i
  print $ p1 k
  print $ p2 k