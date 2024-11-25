module Main where

-- import Data.List

import Debug.Trace
idtrace x = trace (show x) x

keypad = ["       ", "   1   ", "  234  ", " 56789 ", "  ABC  ", "   D   ", "       "]

type Pos = (Int, Int)

key (x, y) = keypad !! x !! y

go :: Char -> Pos -> Pos
go 'U' (x, y) = (x - 1, y)
go 'D' (x, y) = (x + 1, y)
go 'L' (x, y) = (x, y - 1)
go 'R' (x, y) = (x, y + 1)

nextKey :: (Pos, String) -> [Char] -> (Pos, String)
nextKey (pos, out) [] = (pos, out ++ [key pos])
nextKey (pos, out) (d:ds) = nextKey (realnpos nk, out) ds
  where npos = go d pos
        nk = key npos
        realnpos ' ' = pos
        realnpos _ = npos

main = do
  cnt <- getContents
  print $ foldl nextKey ((3, 1), "") $ lines cnt

