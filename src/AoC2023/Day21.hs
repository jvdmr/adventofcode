{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts, TypeFamilies #-}
module AoC2023.Day21
  ( part1
  , part2
  ) where

import Data.List (transpose)
import Data.List.Split (splitOn)
import qualified Data.Map as M

import AoC (Solver)
import AoC.Util (cartesianWith, count)
import AoC.Grid
import AoC.Dijkstra
import AoC.Trace

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

type GridSize = Coord Int
type StartCoord = GCoord
data GardenType = Walled | Open
  deriving (Eq, Show)
data Garden = Garden { grid :: Grid Tile, start :: StartCoord, typ :: GardenType }
  deriving (Eq, Show)

isPlot :: Garden -> GCoord -> Bool
isPlot g c = inGrid g' c && Plot == g' ! c
  where g' = grid g

plots :: Garden -> [GCoord]
plots g = filter (isPlot g) $ coords $ grid g

garden :: GardenType -> Grid Char -> Garden
garden t g = Garden g' s t
  where g' = mapG tile g
        s = head $ filter ((== 'S') . (g !)) $ coords g

siz :: Garden -> GridSize
siz = size . grid

gridmod :: GridSize -> GCoord -> GCoord
gridmod (mx, my) (x, y) = (mod x mx, mod y my)

griddiv :: GridSize -> GCoord -> GCoord
griddiv (mx, my) (x, y) = (div x mx, div y my)

-- <!-- Dijkstra stuff
instance GName GCoord
instance GGraph Garden where
  type GNodeName Garden = GCoord
  nodes g = plots g
  edges g c | typ g == Walled = [(d, 1) | d <- map (flip go c) [D, R, U, L], isPlot g d]
            | otherwise = [(d, 1) | d <- map (flip go c) [D, R, U, L], isPlot g $ gridmod (siz g) d]
-- -->
 
reach :: Garden -> Int -> Int
reach g n = length reachables
  where reachables = filter dg $ map (add s) $ cartesianWith add diag $ map neg diag'
        diag = zip [0..n] $ reverse [0..n]
        diag' = zip [0..n] [0..n]
        dg = dijkstraMax g s (Dist n)
        s = start g

part1 :: Solver
part1 input = show $ map (solve . read) steps
  where solve = rtrace $ reach g'
        g' = garden Walled $ Grid input'
        [steps, _, input'] = splitOn [""] $ lines input

part2 :: Solver
part2 input = show $ map (solve . read) steps
  where solve = rtrace $ reach g'
        g' = garden Open $ Grid input'
        [_, steps, input'] = splitOn [""] $ lines input

