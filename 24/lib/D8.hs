module D8 where

p1 i = ()

p2 i = ()

-- assignment too complicated, wtf am i supposed to even do how does this work

p i = do
  print $ p1 k
  print $ p2 k
  where
    k = lines i
