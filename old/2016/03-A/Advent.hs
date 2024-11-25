module Main where

-- import Data.List

import Debug.Trace
idtrace x = trace (show x) x

possible :: [Int] -> Bool
possible [a, b, c] = a + b > c && a + c > b && b + c > a

main = do
  cnt <- getContents
  print $ length $ filter possible $ map (map read . words) $ lines cnt

