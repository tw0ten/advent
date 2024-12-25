module D where

r p1 p2 k = (show $ p1 k, show $ p2 k)

loop 0 _ i = i
loop x e i = loop (x - 1) e $ e i

enlist x = [x]

diff n1 n2 = abs $ n1 - n2

parse _ [] = True
parse [] _ = False
parse (x : xs) (y : ys) = x == y && parse xs ys

readInt i = read i :: Int

rest = drop 1

intLen _ 0 = 0
intLen b n = 1 + intLen b (n `div` b)

split d i = let (r1, r2) = span d i in (r1, rest r2)

splitElem d = split ((/=) d)

splitL _ [] = []
splitL c s =
  let (r, rs) = split c s
   in r : splitL c rs

type V2 a = (a, a)

type V2I = V2 Int

data Direction = UP | RIGHT | DOWN | LEFT

instance Show Direction where
  show UP = "^"
  show DOWN = "v"
  show LEFT = "<"
  show RIGHT = ">"

parseDir c = l [UP, DOWN, LEFT, RIGHT]
  where
    l = lookup c . map (\c -> (show c, c))

dirV UP = (0, 1)
dirV RIGHT = (1, 0)
dirV DOWN = (0, -1)
dirV LEFT = (-1, 0)

mmap f = map (map f)

ib x = if x then 1 else 0

count x = length . filter x

zipV i = [[(v, (x, y)) | (v, x) <- zip r [0 ..]] | (y, r) <- zip [0 ..] i]
