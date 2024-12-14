module Main where

import D0 (days)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  args <- getArgs
  mapM_ (\(f, n) -> i n f)
    . filter (\(_, n) -> show n `elem` args)
    $ zip days [1 ..]
  where
    i x f = do
      y <- readFile $ "i/" ++ z
      let (o1, o2) = f y
      putStr $ " " ++ z
      hFlush stdout
      writeFile ("o/" ++ z) $ o1 ++ "\n" ++ o2
      where
        z = show x
