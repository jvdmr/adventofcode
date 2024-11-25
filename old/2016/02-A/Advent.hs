module Main where

-- import Data.List

import Debug.Trace
idtrace x = trace (show x) x

keypad = ["123", "456", "789"]

type Pos = (Int, Int)

key (x, y) = keypad !! x !! y

go :: Char -> Pos -> Pos
go 'U' (x, y) = (max 0 (x - 1), y)
go 'D' (x, y) = (min 2 (x + 1), y)
go 'L' (x, y) = (x, max 0 (y - 1))
go 'R' (x, y) = (x, min 2 (y + 1))

nextKey :: (Pos, String) -> [Char] -> (Pos, String)
nextKey (pos, out) [] = (pos, out ++ [key pos])
nextKey (pos, out) (d:ds) = nextKey (go d pos, out) ds

main = do
  cnt <- getContents
  print $ foldl nextKey ((1, 1), "") $ lines cnt

