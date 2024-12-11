module D3 where

import Data.Char (isDigit)

rest = drop 1

parse _ [] = True
parse [] _ = False
parse (x : xs) (y : ys) = x == y && parse xs ys

next i = r0 "mul("
  where
    readInt s = if b then c : readInt (rest s) else ""
      where
        c = s !! 0
        b = isDigit c

    r0 s = if parse i s then r1 (drop (length s) i) else 0
    r4 x y i = if parse i ")" then (read x) * (read y) else 0

    r1 i = let x = readInt i in if x == "" then 0 else r2 x (drop (length x) i)
    r2 x i = if parse i "," then r3 x (rest i) else 0
    r3 x i = let y = readInt i in if y == "" then 0 else r4 x y (drop (length y) i)

p1 i = sum $ loop i
  where
    loop "" = []
    loop i = next i : loop (rest i)

p2 i = sum $ loop True i
  where
    loop _ "" = []
    loop s i = (if s then next i else 0) : loop cx (rest i)
      where
        cx
          | parse i "do()" = True
          | parse i "don't()" = False
          | otherwise = s

p i = do
  print $ p1 i
  print $ p2 i
