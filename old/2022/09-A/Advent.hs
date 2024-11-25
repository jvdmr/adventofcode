module Main where

-- import Data.List
import Data.List.Split

import Debug.Trace
idtrace x = trace (show x) x

type Pos = (Int, Int)

type Rope = (Pos, Pos)

s = (0, 0)

uniq [] = []
uniq [a] = [a]
uniq (a:b:rst) | a == b = uniq (b:rst)
               | otherwise = a:uniq (b:rst)

rep :: Int -> a -> [a]
rep n x = take n $ repeat x

up = (0, 1)
left = (-1, 0)
down = (0, -1)
right = (1, 0)

dir "U" = up
dir "R" = right
dir "D" = down
dir "L" = left

parseMoves :: [[String]] -> [Pos]
parseMoves [] = []
parseMoves ([d, n]:rest) = (rep (read n) $ dir d) ++ parseMoves rest

move :: [Pos] -> Rope -> [Pos] -> [Pos]
move been _ [] = been
move been ((hx, hy), (tx, ty)) ((x, y):rest) = move (t:been) (h, t) rest
  where h = (hx + x, hy + y)
        t = (tx', ty')
        dx = hx + x - tx
        dy = hy + y - ty
        tx' | abs dx > 1 = tx + x
            | abs dx + abs dy > 2 = tx + (dx `div` abs dx)
            | otherwise = tx
        ty' | abs dy > 1 = ty + y
            | abs dx + abs dy > 2 = ty + (dy `div` abs dy)
            | otherwise = ty

main = do
  cnt <- getContents
  print $ length $ uniq $ sort $ idtrace $ move [s] (s, s) $ parseMoves $ map (splitOn " ") $ lines cnt

