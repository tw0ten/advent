module D6 where

data Direction = UP | RIGHT | DOWN | LEFT

turnRight UP = RIGHT
turnRight RIGHT = DOWN
turnRight DOWN = LEFT
turnRight LEFT = UP

dirVec UP = (0, 1)
dirVec RIGHT = (1, 0)
dirVec DOWN = (0, -1)
dirVec LEFT = (-1, 0)

data Cell = GUARD Direction | WALL | EMPTY

p1 i =
  length $
    l
      ( [ (i, j, v)
        | (i, row) <- zip [0 ..] i,
          (j, v) <- zip [0 ..] row
        ]
      )
      []
  where
    (h, w) = (length i, length (i !! 0))
    f _ = True
    l i visited = map f i
      where
        f a = ()
    id k i j
      | i < 0 || j < 0 || i >= h || j >= w = ()
      | otherwise = k !! i !! j

p2 i = ()

p i = do
  let k = map (map p) $ lines i
  print $ p1 k
  print $ p2 k
  where
    p '^' = GUARD UP
    p '#' = WALL
    p _ = EMPTY
