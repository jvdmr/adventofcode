module Main where

import Data.List
import Data.List.Split

import Debug.Trace
idtrace x = trace (show x) x

main = do
  cnt <- getContents
  print $ foldl (+) 0 $ take 3 $ reverse $ sort $ map (foldl (+) 0 . map read) $ splitOn [""] $ lines cnt

