module Main where

import M (days)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  a <- getArgs
  mapM_ i
    . filter (\(_, n) -> show n `elem` a)
    $ zip days [1 ..]
  where
    i (f, x) = do
      y <- readFile $ "i/" ++ z
      let (o1, o2) = f y
      writeFile
        ("o/" ++ z)
        $ o1 ++ "\n" ++ o2
      putStr $ " " ++ z
      hFlush stdout
      where
        z = show x
