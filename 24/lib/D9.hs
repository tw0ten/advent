module D9 where

p1 i = ()

p2 i = ()

p i = do
  print $ p1 k
  print $ p2 k
  where
    k = map (\c -> read [c] :: Int) . reverse . drop 1 $ reverse i
