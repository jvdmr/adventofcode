module Main where

import Data.List
import Data.List.Split

import Debug.Trace
idtrace x = trace (show x) x

main = do
  cnt <- getContents
  print $ last $ sort $ map (foldl (+) 0 . map read) $ splitOn [""] $ lines cnt

