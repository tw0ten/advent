module D2 where

input = do
  input <- readFile "i/2"
  return $ map (\x -> map (\y -> read y :: Int) x) $ map words $ lines input

p1 :: [[Int]] -> IO ()
p1 i = do
  print $ length . filter id $ map (\z -> v (\(x, y) -> x < y) z || v (\(x, y) -> x > y) z) i
  where
    diff n1 n2 = abs (n1 - n2)
    v op (p : c : r) = op (p, c) && diff p c <= 3 && v op (c : r)
    v _ _ = True

p2 i = do
  print . length . filter id $ map isValid i
  where
    isValid arr = or $ map (\arr -> or [v (\x y -> x < y) arr, v (\x y -> x > y) arr]) (removeOne arr)
    removeOne xs = [take i xs ++ drop (i + 1) xs | i <- [0 .. length xs - 1]]
    diff n1 n2 = abs (n1 - n2)
    v op (p : c : r) = op p c && diff p c <= 3 && v op (c : r)
    v _ _ = True

main = do
  i <- input
  putStrLn "p1"
  p1 i
  putStrLn "p2"
  p2 i
