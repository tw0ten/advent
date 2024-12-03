module D3 where

rest = drop 1

readInt s = if is then c : readInt (rest s) else ""
  where
    c = head s
    is = or $ map (\i -> i == c) nums
      where
        nums = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

parse _ [] = True
parse [] _ = False
parse (x : xs) (y : ys) = x == y && parse xs ys

p1 i = sum $ map read . filter (\c -> not (c == "")) $ loop i
  where
    loop "" = []
    loop i = next i : loop (rest i)
      where
        next i = if parse i "mul(" then readNum (drop (length "mul(") i) else ""
        readNum i = let x = readInt i in if x == "" || not (parse (drop (length x) i) ",") then "" else secondNum x (drop (length x + 1) i)
        secondNum x i = let y = readInt i in if not (y == "") && parse (drop (length y) i) ")" then show $ (read x :: Int) * (read y :: Int) else ""

p2 i = sum $ map read . filter (\c -> not (c == "")) $ loop True i
  where
    loop _ "" = []
    loop s i = (if s then next i else "") : loop cx (rest i)
      where
        cx = if parse i "do()" then True else if parse i "don't()" then False else s
        next i = if parse i "mul(" then readNum (drop (length "mul(") i) else ""
        readNum i = let x = readInt i in if x == "" || not (parse (drop (length x) i) ",") then "" else secondNum x (drop (length x + 1) i)
        secondNum x i = let y = readInt i in if not (y == "") && parse (drop (length y) i) ")" then show $ (read x :: Int) * (read y :: Int) else ""

-- not proud

p i = do
  print $ p1 i
  print $ p2 i
