module Main where

import Data.List
import Data.List.Split

import Debug.Trace
idtrace x = trace (show x) x

flatten :: [[a]] -> [a]
flatten = foldl (++) []

horizontalMirror :: [String] -> Int
horizontalMirror (p:pattern) = mirror' ([p], pattern)
  where mirror' (_, []) = 0
        mirror' (stack, rest) | and $ zipWith (==) stack rest = length stack
                              | otherwise = mirror' ((head rest):stack, tail rest)

verticalMirror :: [String] -> Int
verticalMirror = horizontalMirror . transpose

mirrors :: [String] -> Int
mirrors pattern = v + h * 100
  where v = verticalMirror pattern
        h = horizontalMirror pattern

main = do
  cnt <- getContents
  print $ sum $ map mirrors $ splitOn [""] $ lines cnt

