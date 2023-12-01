module Main where

import Data.List
import Data.Char

import Debug.Trace
idtrace x = trace (show x) x

fldigits :: [Char] -> Int
fldigits l = read $ [head ds, last ds]
  where ds = filter isDigit l

main = do
  cnt <- getContents
  print $ sum $ map fldigits $ lines cnt

