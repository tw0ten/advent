module D7 where

import Control.Monad (replicateM)

data Op = MUL | PLUS | CONCAT

m v e MUL = v * e
m v e PLUS = v + e
m v e CONCAT = v * (10 ^ l e) + e
  where
    l 0 = 0
    l n = 1 + l (n `div` 10)

s os (r, e) = if r `elem` so then r else 0
  where
    so = map (sf 0 e) $ replicateM (length e) os
      where
        sf v (e : es) (o : os) = sf (m v e o) es os
        sf v _ _ = v

p1 i = sum $ map (s [MUL, PLUS]) i

p2 i = sum $  map (s [MUL, PLUS, CONCAT]) [] -- i

p i = do
  print $ p1 k
  print $ p2 k
  where
    k = map (\(r, e) -> (read r :: Int, s0 $ drop 1 e)) $ map (\c -> (s (\c -> not (c == ':')) c)) $ lines i
      where
        s d i = (takeWhile d i, drop 1 $ dropWhile d i)
        s0 c = if length c > 0 then (read n :: Int) : s0 (drop (length n + 1) c) else []
          where
            n = takeWhile (\c -> not (c == ' ')) c
