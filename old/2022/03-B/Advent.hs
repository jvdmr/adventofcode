module Main where

import Data.List
import Data.List.Split

import Debug.Trace
idtrace x = trace (show x) x

indexOf :: (Eq a) => a -> [a] -> Int
indexOf a ls = indexOfHelp 0 a ls
  where indexOfHelp _ a [] = -1
        indexOfHelp i a (l:ls) | a == l = i
                               | otherwise = indexOfHelp (i + 1) a ls

prio :: Char -> Int
prio c = indexOf c " abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

findCommon :: (Eq a) => [[a]] -> a
findCommon [la, lb, lc] = head $ filter (\a -> elem a lb && elem a lc) la

main = do
  cnt <- getContents
  print $ sum $ map (prio . findCommon) $ chunksOf 3 $ lines cnt

