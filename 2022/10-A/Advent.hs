module Main where

import Data.List

import Debug.Trace
idtrace x = trace (show x) x

cpu x (["noop"]:nxt) = x:cpu x nxt
cpu x (["addx", n]:nxt) = x:x:cpu (x + (read n)) nxt

main = do
  cnt <- getContents
  let signals = 1:(cpu 1 $ map words $ lines cnt)
      signal n = n * signals !! n
  print $ sum $ map signal $ take 6 [20,60..]

