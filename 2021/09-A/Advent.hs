module Main where

import Data.List

import Debug.Trace
idtrace x = trace (show x) x
tagtrace tag x = trace (tag ++ show x) x

flatten = foldl (++) []

findLow :: [[Int]] -> [Int]
findLow iss = flatten $ map (\x -> map (isLow x) [0..maxY]) [0..maxX]
  where maxX = (length iss) - 1
        maxY = (length $ head iss) - 1
        lookup x y | x < 0 || y < 0 || x > maxX || y > maxY = 999
                   | otherwise = (iss !! x) !! y
        isLow x y | val < up && val < down && val < left && val < right = val
                  | otherwise = -1
          where val = lookup x y
                up = lookup (x - 1) y
                down = lookup (x + 1) y
                left = lookup x (y - 1)
                right = lookup x (y + 1)

main = do
  cnt <- getContents
  print $ foldl (+) 0 $ map (+ 1) $ findLow $ map (map (read . (:[]))) $ lines cnt

