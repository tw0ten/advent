module D14 where

import D (loop, notEq, readInt, rest, splitElem,r)

p1 (i, (w, h)) = q $ loop 100 f i
  where
    q i =
      product $
        map
          length
          [ f (\(x, y) -> x < hw && y < hh) i,
            f (\(x, y) -> x > hw && y > hh) i,
            f (\(x, y) -> x < hw && y > hh) i,
            f (\(x, y) -> x > hw && y < hh) i
          ]
      where
        hw = w `div` 2
        hh = h `div` 2
        f p = filter (fi p)
          where
            fi p (i, _) = p i
    f i = map m $ i
      where
        m ((x, y), v) =
          let (vx, vy) = v
           in (((w + x + vx) `mod` w, (h + y + vy) `mod` h), v)

-- how the fuck does the christmas tree look? what/???
-- am i supposed to do it manually?
p2 (i, (w, h)) = ()

p i = r p1 p2 k
  where
    k = (map f . map (splitElem ' ') $ lines i, (101, 103))
      where
        f i = let (p, v) = i in (pa p, pa v)
          where
            pa i =
              let (x, y) = splitElem ',' . rest $ dropWhile (notEq '=') i
               in (readInt x, readInt y)
