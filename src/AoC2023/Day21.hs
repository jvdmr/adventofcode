{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts, TypeFamilies #-}
module AoC2023.Day21
  ( part1
  , part2
  ) where

import qualified Data.Map as M

import AoC (Solver, count, bfsTiers)
import AoC.Grid
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

isPlot :: Grid Tile -> GCoord -> Bool
isPlot g c = Plot == g ! c

plots :: Grid Tile -> [GCoord]
plots g = filter (isPlot g) $ coords g

type GridSize = Coord Int
type StartCoord = GCoord
type Garden = (Grid Tile, StartCoord)
data GardenType = Walled | Open

start :: Grid Char -> StartCoord
start g = head $ filter ((== 'S') . (g !)) $ coords g

garden :: Grid Char -> Garden
garden g = (g', start g)
  where g' = mapG tile g

gridmod :: GridSize -> GCoord -> GCoord
gridmod (mx, my) (x, y) = (mod x mx, mod y my)

edges :: GardenType -> Grid Tile -> (GCoord -> [GCoord])
edges Walled g = nbs'
  where nbs = M.fromList [(c, [nb | nb <- map (flip go c) [D, R, U, L], inGrid g nb, isPlot g nb]) | c <- plots g]
        nbs' = (M.!) nbs
edges Open g = nbs'
  where nbs = M.fromList [(c, [nb | nb <- map (flip go c) [D, R, U, L], isPlot g $ gm nb]) | c <- plots g]
        gm = gridmod $ size g
        nbs' c = let c' = gm c in let cdiff = add c $ neg c' in map (add cdiff) $ nbs M.! c'

reach :: GardenType -> Garden -> (Int -> Int)
reach t (g, is) n = sum $ map (length . fst) $ filter (even . (-) n . snd) $ takeWhile ((<= n) . snd) distances
  where neighbors = edges t g
        distances = bfsTiers neighbors is

part1 :: Solver
part1 input = show $ map solve [6, 64]
  where solve = rtrace $ reach Walled g'
        g' = garden $ Grid $ lines input

part2 :: Solver
-- part2 input = show $ map solve [6, 10, 50, 100, 500, 1000, 5000, 26501365]
part2 input = show $ map solve [6, 10, 50, 100, 500]
  where solve = rtrace $ reach Open g'
        g' = garden $ Grid $ lines input

