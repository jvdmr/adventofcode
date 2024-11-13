module Main where

import Data.List
import Data.List.Split

import Debug.Trace
idtrace x = trace (show x) x

cantor x y = (x + y) * (x + y + 1) `div` 2 + y

codes = iterate f 20151125
  where f n = n * 252533 `mod` 33554393

calculate [r, c] = codes !! i
  where i = cantor (r - 1) (c - 1)

main = do
  cnt <- getContents
  print $ map (calculate . map read . splitOn " ") $ lines cnt

