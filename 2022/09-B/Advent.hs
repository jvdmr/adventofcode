module Main where

import Data.List
import Data.List.Split

import Debug.Trace
idtrace x = trace (show x) x

type Pos = (Int, Int)

type Rope = [Pos]

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

moveRope :: Rope -> Rope
moveRope [t] = [t]
moveRope (h@(hx, hy):(tx, ty):rest) = h:moveRope (t:rest)
  where t = (tx', ty')
        dx = hx - tx
        dy = hy - ty
        x = dx `div` abs dx
        y = dy `div` abs dy
        tx' | abs dx > 1 = tx + x
            | abs dx + abs dy > 2 = tx + x
            | otherwise = tx
        ty' | abs dy > 1 = ty + y
            | abs dx + abs dy > 2 = ty + y
            | otherwise = ty

move :: [Pos] -> Rope -> [Pos] -> [Pos]
move been _ [] = been
move been ((hx, hy):r) ((x, y):rest) = move (t:been) rope rest
  where h = (hx + x, hy + y)
        rope = moveRope (h:r)
        t = last rope

main = do
  cnt <- getContents
  print $ length $ uniq $ sort $ idtrace $ move [s] (rep 10 s) $ parseMoves $ map (splitOn " ") $ lines cnt

