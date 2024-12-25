module D24 (p) where

import D (r, rest, splitElem)
import Data.List (partition, sortBy, sortOn)

p1 (s, i) = f $ re s i
  where
    f = ifb . reverse . map snd . sortOn fst . filter (fi . fst)
      where
        fi ('z' : _) = True
        fi _ = False
        ifb (b : r) = ifb r + if b then 2 ^ length r else 0
        ifb _ = 0
    re s (i : r) = case f i of
      Just a -> re (a : s) r
      _ -> re s $ r ++ [i]
      where
        f ((i1, i2), o, r) =
          case (f i1, f i2) of
            (Just i1, Just i2) -> Just (r, o i1 i2)
            _ -> Nothing
          where
            f n = lookup n s
    re s _ = s

p2 (s, i) = ()
  where
    a = 4

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
