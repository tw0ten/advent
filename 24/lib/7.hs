module D7 (p) where

import Control.Monad (replicateM)
import D (r, readInt, rest, splitElem)

data Op = MUL | PLUS | CONCAT

s os (r, e) = if r `elem` so then r else 0
  where
    so = map (sf 0 e) $ replicateM (length e) os
      where
        sf v (e : es) (o : os) = sf (m v e o) es os
          where
            m v e MUL = v * e
            m v e PLUS = v + e
            m v e CONCAT = v * (10 ^ l e) + e
              where
                l 0 = 0
                l n = 1 + l (n `div` 10)
        sf v _ _ = v

p1 i = sum $ map (s [MUL, PLUS]) i

p2 i = sum $ map (s [MUL, PLUS, CONCAT]) i

p i = r p1 p2 k
  where
    k = map (\(r, e) -> (readInt r, s0 $ rest e)) $ map (\c -> (splitElem ':' c)) $ lines i
      where
        s0 c = if length c > 0 then readInt n : s0 (drop (length n + 1) c) else []
          where
            n = takeWhile (\c -> not $ c == ' ') c
