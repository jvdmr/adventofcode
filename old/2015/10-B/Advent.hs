module Main where

import Data.List hiding (insert)

import Debug.Trace
idtrace x = trace (show x) x

lookandsaystep :: String -> String
lookandsaystep [] = ""
lookandsaystep (x:rst) = show l ++ [x] ++ lookandsaystep otherRst
  where l = 1 + (length $ takeWhile (== x) rst)
        otherRst = dropWhile (== x) rst

lookandsay :: Int -> String -> String
lookandsay n s = iterate lookandsaystep s !! n 

rounds = 50

main = do
  cnt <- getContents
  print $ lookandsay rounds $ head $ lines cnt
  print $ length $ lookandsay rounds $ head $ lines cnt

