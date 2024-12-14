module D5 where

import D0 (readInt, split)
import Data.List (sortBy)

f fi (l1, l2) = sum . map (\c -> readInt $ c !! (length c `div` 2)) $ fi (map (so l1) l2) l2
  where
    so r = sortBy (com r)
      where
        com r x y
          | (x, y) `elem` r = LT
          | (y, x) `elem` r = GT
          | otherwise = EQ

p1 (l1, l2) = f fi (l1, l2)
  where
    fi i s = filter (\c -> c `elem` s) i

p2 (l1, l2) = f fi (l1, l2)
  where
    fi i s = filter (\c -> not $ c `elem` s) i

p i = do
  print $ p1 k
  print $ p2 k
  where
    k =
      let (l1, l2) = split (\c -> not $ c == "") $ lines i
       in (map (\c -> split (\c -> not $ c == '|') c) l1, map s0 l2)
      where
        s0 c = if length c > 0 then n : s0 (drop (length n + 1) c) else []
          where
            n = takeWhile (\c -> not $ c == ',') c
