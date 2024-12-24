module D5 where

import D (r, readInt, splitElem)
import Data.List (sortBy)

f fi (l1, l2) = sum . map (\c -> readInt $ c !! (length c `div` 2)) $ fi (map (so l1) l2) l2
  where
    so r = sortBy (com r)
      where
        com r x y
          | (x, y) `elem` r = LT
          | (y, x) `elem` r = GT
          | otherwise = EQ

p1 i = f fi i
  where
    fi s = filter (`elem` s)

p2 i = f fi i
  where
    fi s = filter (not . (`elem` s))

p i = r p1 p2 k
  where
    k =
      let (l1, l2) = splitElem "" $ lines i
       in (map (splitElem '|') l1, map s0 l2)
      where
        s0 c = if length c > 0 then n : s0 (drop (length n + 1) c) else []
          where
            n = takeWhile ((/=) ',') c
