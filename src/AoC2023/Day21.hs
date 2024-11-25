{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC2023.Day21
  ( part1
  , part2
  ) where

import Data.List

import AoC (Solver, andF, uniq)
import AoC.Grid
import AoC.Trace (idtrace)

type GCoord = Coord Int

data Tile = Plot
          | Rock
          deriving (Eq)

instance Show Tile where
  show Plot = " "
  show Rock = "#"

tile :: Char -> Tile
tile '.' = Plot
tile '#' = Rock
tile 'S' = Plot

garden :: Grid Char -> Grid Tile
garden = mapG tile

start :: Grid Char -> GCoord
start g = head $ filter ((== 'S') . (g !)) $ coords g

goAll :: GCoord -> [GCoord]
goAll c = map (flip go c) [D, R, U, L]

type Reachable = (Grid Tile, [GCoord])

step :: Reachable -> Reachable
step (g, cs) = (g, nub $ concat $ map (filter (flip andF [inGrid g, (== Plot) . (g !)]) . goAll) cs)

reach :: (Reachable -> Reachable) -> Grid Char -> [Int]
reach stepF gc = map (length . snd) cs
  where g = garden gc
        s = start gc
        cs = (iterate stepF (g, [s]))

reachP1 :: Grid Char -> [Int]
reachP1 g = map (rs !!) [6, 64]
  where rs = reach step g

part1 :: Solver
part1 = show . map idtrace . reachP1 . Grid . lines

gridmod :: Grid a -> GCoord -> GCoord
gridmod (Grid g) (x, y) = (mod x mx, mod y my)
  where my = length g
        mx = length (head g)

stepInf :: Reachable -> Reachable
stepInf (g, cs) = (g, uniq $ sort $ concat $ map (filter (flip andF [(== Plot) . (g !)] . gridmod g) . goAll) cs)

reachP2 :: Grid Char -> [Int]
reachP2 g = map (rs !!) [6, 10, 50, 100, 500, 1000, 5000, 26501365]
  where rs = reach stepInf g

part2 :: Solver
part2 = show . map idtrace . reachP2 . Grid . lines

