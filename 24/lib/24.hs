module D24 where

import D (r, rest, splitElem)

p1 (s, i) = f $ re s i
  where
    f = map snd . filter (\(n, _) -> fi n)
      where
        fi ('z' : _) = True
        fi _ = False
    re s [] = s
    re s i = re (so s $ i !! 0) $ rest i
      where
        so s i = ("z", True) : s

p2 i = ()

p i = r p1 p2 k
  where
    k =
      let (s, p) = splitElem "" $ lines i
       in (map (b . splitElem ':') s, map f p)
      where
        b (a, " 1") = (a, True)
        b (a, " 0") = (a, False)
        f i =
          let [i1, o, i2, _, r] = words i
           in ((i1, i2), op o, r)
          where
            op "AND" = (&&)
            op "OR" = (||)
            op "XOR" = (/=)
