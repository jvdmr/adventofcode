module Main where

-- import Data.List

import Debug.Trace
idtrace x = trace (show x) x
ftrace f x = trace (f x) x

data Direction = N | E | S | W
               deriving (Eq, Show)

type Tree = (Int, [Direction])

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
reachTree d before [t@(h, ds)] | isEmpty $ filter ((>= h) . fst) before = [(h, d:ds)]
                               | otherwise = [t]
reachTree d before (t@(h, ds):after) | isEmpty $ filter ((>= h) . fst) before = (h, d:ds):reachTree d (t:before) after
                                     | otherwise = t:reachTree d (t:before) after

visible :: Forest -> Direction -> Forest
visible forest d = map (reachTree d []) forest

allSides = [N, E, S, W]

visibleTrees :: Forest -> Forest
visibleTrees forest = foldl (visible . rotate) forest allSides

isVisible :: Tree -> Bool
isVisible (_, ds) = not $ isEmpty ds

showVisible :: TreeLine -> String
showVisible tl = concat $ map v tl
  where v t | isVisible t = "#"
            | otherwise = " "

main = do
  cnt <- getContents
  print $ length $ filter isVisible $ concat $ map (ftrace showVisible) $ visibleTrees $ map (map $ prep . read . (:[])) $ lines cnt

