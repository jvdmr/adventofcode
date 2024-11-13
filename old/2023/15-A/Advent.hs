module Main where

import Data.List
import Data.List.Split (splitOn)
import Data.Char

import Debug.Trace
idtrace x = trace (show x) x

hash :: Int -> [Char] -> Int
hash i [] = i
hash i (c:rest) = hash i' rest
  where i' = ((i + ord c) * 17) `mod` 256

main = do
  cnt <- getContents
  print $ sum $ map (hash 0) $ splitOn "," $ head $ lines cnt

