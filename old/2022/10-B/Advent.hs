module Main where

-- import Data.List
import Data.List.Split

import Debug.Trace
idtrace x = trace (show x) x

cpu x [] = [x]
cpu x (["noop"]:nxt) = x:cpu x nxt
cpu x (["addx", n]:nxt) = x:x:cpu (x + (read n)) nxt

main = do
  cnt <- getContents
  let signals = (cpu 1 $ map words $ lines cnt)
      sprite n = signals !! n
      crt n | abs (sprite n - mod n 40) <= 1 = '#'
            | otherwise = ' '
  print $ map (idtrace . (++"\n")) $ chunksOf 40 $ map crt [1..240]

