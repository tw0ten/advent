module D0 where

p1 i = ()

p2 i = ()

p i = do
  print $ p1 k
  print $ p2 k
  where
    k = i
