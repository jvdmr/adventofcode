module Main where

import Data.List

import Debug.Trace
idtrace x = trace (show x) x

main = do
  cnt <- getContents
  print $ lines cnt
