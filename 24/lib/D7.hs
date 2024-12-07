module D7 where

import Control.Monad (replicateM)

p1 i = sum $ map s i
  where
    s (r, e) = if r `elem` so e then r else 0
    so e = map (sf 0 e) $ replicateM (length e) os
    os = ['*', '+']
    sf v (e : es) (o : os) = sf (m v e o) es os
      where
        m v e '*' = v * e
        m v e '+' = v + e
    sf v _ _ = v

p2 i = sum $ map s i
  where
    s (r, e) = if r `elem` so e then r else 0
    so e = map (sf 0 e) $ replicateM (length e) os
    os = ["*", "+", "||"]
    sf v (e : es) (o : os) = sf (m v e o) es os
      where
        m v e "*" = v * e
        m v e "+" = v + e
        m v e "||" = (v * (10 ^ (l e))) + e
          where
            l 0 = 0
            l n = 1 + (l (n `div` 10))
    sf v _ _ = v

p i = do
  let k = map (\(r, e) -> (read r :: Int, s0 $ drop 1 e)) $ map (\c -> (s (\c -> not (c == ':')) c)) $ lines i
  print $ p1 k
  print $ p2 k
  where
    s d i = (takeWhile d i, drop 1 $ dropWhile d i)
    s0 c = if length c > 0 then (read n :: Int) : s0 (drop (length n + 1) c) else []
      where
        n = takeWhile (\c -> not (c == ' ')) c
