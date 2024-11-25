module Main where

-- import Data.List
import Data.List.Split

import Debug.Trace
idtrace x = trace (show x) x

tuplify [x, y] = (x, y)

expandRange :: String -> (Int, Int)
expandRange = tuplify . map read . splitOn "-"

parseTasks :: String -> [(Int, Int)]
parseTasks = map expandRange . splitOn ","

findWhollyContaining :: (Ord a) => [(a, a)] -> Bool
findWhollyContaining [(a, b), (c, d)] | a <= c && b >= c = True
                                      | a <= d && b >= d = True
                                      | a >= c && b <= d = True
                                      | otherwise = False

main = do
  cnt <- getContents
  print $ length $ filter findWhollyContaining $ map parseTasks $ lines cnt

