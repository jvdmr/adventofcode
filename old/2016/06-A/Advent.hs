module Main where

import Data.Ord
import Data.List

import Debug.Trace
idtrace x = trace (show x) x

main = do
  cnt <- getContents
  print $ map (head . head . reverse . sortBy (comparing length) . group . sort) $ transpose $ lines cnt

