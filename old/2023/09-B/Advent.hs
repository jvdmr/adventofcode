module Main where

-- import Data.List
import Data.List.Split (splitOn)

import Debug.Trace
idtrace x = trace (show x) x

diffs :: [Int] -> [Int]
diffs [] = []
diffs [a] = []
diffs (a:b:rest) = (b - a):diffs (b:rest)

prevInSeries :: [Int] -> Int
prevInSeries ns | all (== 0) ns = 0
                | otherwise = head ns - prevInSeries (diffs ns)

main = do
  cnt <- getContents
  print $ sum $ map (prevInSeries . map read . splitOn " ") $ lines cnt

