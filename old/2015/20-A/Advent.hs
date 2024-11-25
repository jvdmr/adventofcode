module Main where

-- import Data.List

import Debug.Trace
idtrace x = trace (show x) x
ftrace f x = trace (f x) x

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

factors :: Int -> [Int]
factors n = nub . concat $ [[x, q] | x <- [1..isqrt n], let (q, r) = divMod n x, r == 0]

presents' = (10 *) . sum . factors

presents n = findIndex (>= n) $ map presents' [0..]

main = do
  cnt <- getContents
  print $ map (presents . read) $ lines cnt

