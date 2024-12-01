module D1 where

import Data.List (sort)

input = do
  input <- readFile "i/1"
  let n = map (\b -> read b :: Int) $ words input
  let (l1, l2) = splitList n
  return (sort l1, sort l2)
  where
    splitList l = (o, e)
      where
        xs = zip l [0 ..]
        o = [x | (x, i) <- xs, odd i]
        e = [x | (x, i) <- xs, even i]

p1 i = do
  let diffs = zipWith diff l1 l2
  print $ sum diffs
  where
    (l1, l2) = i
    diff n1 n2 = abs (n1 - n2)

p2 i = do
  let n = map (\b -> b * (count b l2)) l1
  print $ sum n
  where
    (l1, l2) = i
    count t (n : ns) = count t ns + if t == n then 1 else 0
    count _ _ = 0

main :: IO ()
main = do
  i <- input
  putStrLn "p1"
  p1 i
  putStrLn "p2"
  p2 i
