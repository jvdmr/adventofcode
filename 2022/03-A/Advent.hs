module Main where

import Data.List

import Debug.Trace
idtrace x = trace (show x) x

indexOf :: (Eq a) => a -> [a] -> Int
indexOf a ls = indexOfHelp 0 a ls
  where indexOfHelp _ a [] = -1
        indexOfHelp i a (l:ls) | a == l = i
                               | otherwise = indexOfHelp (i + 1) a ls

prio :: Char -> Int
prio c = indexOf c " abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

splitInHalf :: [a] -> ([a], [a])
splitInHalf ls = (take half ls, drop half ls)
  where half = length ls `div` 2

findCommon :: (Eq a) => [a] -> [a] -> a
findCommon la lb = head $ filter (flip elem lb) la

main = do
  cnt <- getContents
  print $ sum $ map (prio . uncurry findCommon . splitInHalf) $ lines cnt

