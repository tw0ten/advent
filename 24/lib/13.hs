module D13 where

import D (r, readInt, rest)

p1 i = sum $ map c i
  where
    (p1, p2) = (3, 1)
    c (((b1x, b1y), (b2x, b2y)), (rx, ry)) = 0

p2 i = i

p i = r p1 p2 k
  where
    s _ [] = []
    s m i =
      let r = takeWhile ((/=) m) i
       in r : s m (drop (length r + 1) i)
    k = map r . s "" $ lines i
      where
        r m = let [a, b, p] = m in ((pb a, pb b), pp p)
          where
            p j i =
              let [x, y] =
                    map (readInt . rest . dropWhile ((/=) j)) $ s ',' i
               in (x, y)
            pb = p '+'
            pp = p '='
