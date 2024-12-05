module D5 where

import Data.List (sortBy)

p1 (l1, l2) = sum $ map (\c -> read (c !! (length c `div` 2)) :: Int) $ (overlap (map (sortWithRules l1) l2) l2)
  where
    overlap i s = filter (\c -> c `elem` s) i

compareWithRules rules x y
  | (x, y) `elem` rules = LT
  | (y, x) `elem` rules = GT
  | otherwise = EQ

sortWithRules rules items =
  sortBy (compareWithRules rules) items

p2 (l1, l2) = sum $ map (\c -> read (c !! (length c `div` 2)) :: Int) $ (overlap (map (sortWithRules l1) l2) l2)
  where
    overlap i s = filter (\c -> not (c `elem` s)) i

p i = do
  print $ p1 k
  print $ p2 k
  where
    k =
      let (l1, l2) = s (\c -> not (c == "")) $ lines i
       in ((map (\c -> s (\c -> not (c == '|')) c) l1), map s0 l2)
      where
        s d i = (takeWhile d i, drop 1 $ dropWhile d i)
        s0 c = case length c of
          0 -> []
          _ -> n : s0 (drop (length n + 1) c)
          where
            n = takeWhile (\c -> not (c == ',')) c
