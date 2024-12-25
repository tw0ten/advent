module D15 where

import D (enlist, mmap, parseDir, r, splitElem)

p1 (im, ii) = sum . map (sum . map (gps . fst) . fi) $ s im ii
  where
    fi = filter (f . snd)
      where
        f BOX = True
        f _ = False
    gps (x, y) = x + 100 * y
    s im ii = [[((4, 1), BOX)]]

p2 i = ()

data Cell = WALL | BOX | ROBOT | EMPTY

p i = r p1 p2 k
  where
    k =
      let (ma, mo) = splitElem "" $ lines i
       in (mmap pc ma, map parseDir $ foldMap enlist mo)
      where
        pc 'O' = BOX
        pc '#' = WALL
        pc '@' = ROBOT
        pc _ = EMPTY
