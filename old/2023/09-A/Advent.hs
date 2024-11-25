module Main where

-- import Data.List
import Data.List.Split (splitOn)

import Debug.Trace
idtrace x = trace (show x) x

diffs :: [Int] -> [Int]
diffs [] = []
diffs [a] = []
diffs (a:b:rest) = (b - a):diffs (b:rest)

nextInSeries :: [Int] -> Int
nextInSeries ns | all (== 0) ns = 0
                | otherwise = last ns + nextInSeries (diffs ns)

main = do
  cnt <- getContents
  print $ sum $ map (nextInSeries . map read . splitOn " ") $ lines cnt

