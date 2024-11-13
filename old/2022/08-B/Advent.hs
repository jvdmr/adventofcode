module Main where

import Data.List

import Debug.Trace
idtrace x = trace (show x) x
ftrace f x = trace (f x) x

data Direction = N | E | S | W
               deriving (Eq, Show)

type Tree = (Int, [Int])

type TreeLine = [Tree]

type Forest = [TreeLine]

prep :: Int -> Tree
prep t = (t, [])

rotate :: [[a]] -> [[a]]
rotate = transpose . map reverse

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _ = False

reachTree :: Direction -> TreeLine -> TreeLine -> TreeLine
reachTree _ _ [] = []
reachTree d before (t@(h, ds):after) = (h, l:ds):reachTree d (t:before) after
  where l = blocked + (length $ takeWhile ((< h) . fst) before)
        blocked | isEmpty $ dropWhile ((< h) . fst) before = 0
                | otherwise = 1

visible :: Forest -> Direction -> Forest
visible forest d = map (reachTree d []) forest

allSides = [N, E, S, W]

visibleTrees :: Forest -> Forest
visibleTrees forest = foldl (visible . rotate) forest allSides

scenicScore :: Tree -> Int
scenicScore (_, ds) = foldl (*) 1 ds

main = do
  cnt <- getContents
  print $ last $ sort $ map scenicScore $ concat $ visibleTrees $ map (map $ prep . read . (:[])) $ lines cnt

