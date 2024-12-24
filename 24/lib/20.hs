module D20 where

import D (mmap, r, zipV)

p1 i = ()

p2 i = i

p i = r p1 p2 k
  where
    k =
      let l = zipV $ lines i
       in (mmap (\(c, v) -> (c == '#', v)) l, f 'S' l, f 'E' l)
      where
        f k i =
          snd
            . head
            . concat
            . map
              (filter (\(c, _) -> k == c))
            $ i
