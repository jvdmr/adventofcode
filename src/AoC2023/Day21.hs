{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts, TypeFamilies #-}
module AoC2023.Day21
  ( part1
  , part2
  ) where

import Data.List (transpose)
import qualified Data.Map as M

import AoC (Solver, cartesianWith, count)
import AoC.Grid
import AoC.Dijkstra
import AoC.Trace

type GCoord = Coord Int
type ICoord = Coord Int
type IGCoord = (ICoord, GCoord)

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

type StartCoord = IGCoord

start :: Grid Char -> StartCoord
start g = ((0, 0), head $ filter ((== 'S') . (g !)) $ coords g)

type Garden = (Grid Tile, StartCoord)

garden :: Grid Char -> Garden
garden g = (g', start g)
  where g' = mapG tile g

data GardenType = Walled | Open

gridmod :: GridSize -> GCoord -> IGCoord
gridmod (mx, my) (x, y) = ((div x mx, div y my), (mod x mx, mod y my))

iadd :: ICoord -> IGCoord -> IGCoord
iadd ica (icb, cb) = (add ica icb, cb)

gadd :: IGCoord -> GCoord -> IGCoord
gadd (ica, ca) cb = (ica, add ca cb)

igadd :: IGCoord -> IGCoord -> IGCoord
igadd (ica, ca) (icb, cb) = (add ica icb, add ca cb)

markGrid :: Grid Tile -> [GCoord] -> Grid Char
markGrid g marks = mapG mark cg
  where cg = coordsG g
        mark c | c `elem` marks = 'O'
               | otherwise = head $ show (g ! c)

markGrid' :: Int -> Grid Tile -> [IGCoord] -> Grid Char
markGrid' n g marks = Grid $ concat [map concat $ transpose [ungrid $ mapG (mark (x, y)) cg | x <- [-n..n]] | y <- [-n..n]]
  where cg = coordsG g
        mark ic c | (ic, c) `elem` marks = 'O'
                  | otherwise = head $ show (g ! c)

type ExtendedTileGrid = (Grid Tile, [Int], [Int])

extend :: Grid Tile -> ExtendedTileGrid
extend g = (g, r, r)
  where r = [-1..1]

extendX :: Grid Tile -> ExtendedTileGrid
extendX g = (g, [0, 1], [0])

extendY :: Grid Tile -> ExtendedTileGrid
extendY g = (g, [0], [0, 1])

-- <!-- Dijkstra stuff
instance GName IGCoord
instance GGraph ExtendedTileGrid where
  type GNodeName ExtendedTileGrid = IGCoord
  nodes (g, xr, yr) = [((x, y), c) | c <- plots g, x <- xr, y <- yr]
  edges (g, xr, yr) (ic, c) = let s = size g in [(d, 1) | d <- map (iadd ic . gridmod s . flip go c) [D, R, U, L], elem (fst $ fst d) xr, elem (snd $ fst d) yr, isPlot g $ snd d]
-- -->
 
reach :: GardenType -> Garden -> Int -> Int
reach Walled (g, is) n = length reachables
-- reach Walled (g, is) n = length $ ftrace (drawGrid . markGrid g . map snd) reachables
  where reachables = filter dg $ map (gadd is) $ cartesianWith add diag $ map neg diag'
        diag = zip [0..n] $ reverse [0..n]
        diag' = zip [0..n] [0..n]
        dg = dijkstraMax (extend g) is (Dist n)
reach Open (g, is@(_, s)) n = length reachables
-- reach Open (g, is@(_, s)) n = length $ ftrace (drawGrid . markGrid' 2 g) reachables
-- tip: manhattan distance to repeat parts outside of the 3x3 square surrounding the original garden will always be == width/height of the original garden because there is a straight path to them
-- this is not true for the test input, though, so calculate distance from one edge to the next to add outside the 3x3 square
-- limit testable coordinates to the 5x5 square ?
  where reachables = filter ((<= (Dist n)) . dg') $ filter (isPlot g . snd) $ map (gm . add s) $ cartesianWith add diag $ map neg diag'
        reachables' = count ((==) (0, 0) . fst) reachables
        (sx, sy) = size g
        max1 i = signum i * (min 1 $ abs i)
        min0 i = Dist $ max 0 $ abs i - 1
        dg' ((ix, iy), c) = let (dx, dy) = ad c in dx * min0 ix + dy * min0 iy + dg ((max1 ix, max1 iy), c)
        nx = (min 2 $ n `div` sx) * sx + n `mod` sx
        ny = (min 2 $ n `div` sy) * sy + n `mod` sy
        diag = zip [0..nx] $ reverse [0..ny]
        diag' = zip [0..nx] [0..ny]
        gm = gridmod $ size g
        dg = dijkstra (extend g) is
        p = plots g
        ad = (M.!) additionalDistances
        additionalDistances = M.fromList $ zip p $ map additionalDistance p
        additionalDistance c = (dijkstraX ((0, 0), c) ((1, 0), c), dijkstraY ((0, 0), c) ((0, 1), c))
        dijkstraX = dijkstra $ extendX g
        dijkstraY = dijkstra $ extendY g

part1 :: Solver
part1 input = show $ map solve [6, 64]
  where solve = rtrace $ reach Walled g'
        g' = garden $ Grid $ lines input

type GridSize = Coord Int

part2 :: Solver
part2 input = show $ map solve [6, 10, 50, 100, 500, 1000, 5000, 26501365]
  where solve = rtrace $ reach Open g'
        g' = garden $ Grid $ lines input

