{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC2023.Day21
  ( part1
  , part2
  ) where

import Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Set as S

import AoC (Solver)
import AoC.Grid
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

igadd :: ICoord -> IGCoord -> IGCoord
igadd ga (gb, b) = (add ga gb, b)

type GridSize = Coord Int
type GridGraph = M.Map GCoord [IGCoord]
type GoEverywhere = IGCoord -> S.Set IGCoord

goAll :: GridGraph -> GoEverywhere
goAll dirmap = goAll'
  where goAll' (gc, c) = S.fromList $ map (igadd gc) $ dirmap M.! c

type Garden = (Grid Tile, StartCoord)

garden :: Grid Char -> Garden
garden g = (g', start g)
  where g' = mapG tile g

markGrid :: Grid Tile -> [GCoord] -> Grid Char
markGrid g marks = mapG mark cg
  where cg = coordsG g
        mark c | c `elem` marks = 'O'
               | otherwise = head $ show (g ! c)

gridMarks :: Int -> Grid Tile -> [(Int, S.Set IGCoord)] -> String
gridMarks n g = intercalate "\n\n" . map (drawGrid . markGrid g . map snd . S.toList . snd) . take n

-- gridMarks :: Int -> Grid Tile -> [S.Set IGCoord] -> String
-- gridMarks n g = intercalate "\n\n" . map (drawGrid . markGrid g . map snd . S.toList) . take n

-- reach :: Garden -> GoEverywhere -> Int -> Int
-- reach (g, s) go' n = fst $ memf (n + 1)
--   where mem = (0, S.empty):(1, S.singleton s):map step (zip mem (tail mem))
-- --         memf x = (ftrace (gridMarks 4 g) $ mem) !! x
--         memf x = mem !! x
--         step ((pl, ps), (_, ps')) = let cs = newcs ps ps' in (pl + S.size cs, cs)
--         newcs ps ps' = flip S.difference (S.union ps ps') $ S.unions $ S.map go' ps'

reach :: Garden -> GoEverywhere -> Int -> GoEverywhere -> Int -> Int
reach (g, s) go' steps go'' n | n `mod` steps == 0 = fst $ memf ((div n steps) + 1)
                              | otherwise = fst $ memf' ((div (n - 1) steps) + 1)
  where mem = (0, S.empty):(1, S.singleton s):map step (zip mem (tail mem))
--         memf x = (ftrace (gridMarks 4 g) $ mem) !! x
        memf x = mem !! x
        mem' = (0, S.empty):(S.size firstcs, firstcs):map step (zip mem' (tail mem'))
--         memf' x = (ftrace (gridMarks 4 g) $ mem') !! x
        memf' x = mem' !! x
        step ((_, ps'), (pl, ps)) = let cs = newcs ps ps' in (pl + S.size cs, cs)
        newcs ps ps' = flip S.difference (S.union ps ps') $ S.unions $ S.map go'' ps
        firstcs = go' s

-- reach :: Garden -> GoEverywhere -> Int -> GoEverywhere -> Int -> Int
-- reach (g, s) go' steps go'' n | n `mod` steps == 0 = S.size $ memf (div n steps)
--                               | otherwise = S.size $ memf' (div (n - 1) steps)
--   where mem = (S.singleton s):map step mem
-- --         memf x = (ftrace (gridMarks 4 g) $ mem) !! x
--         memf x = mem !! x
--         mem' = firstcs:map step mem'
-- --         memf' x = (ftrace (gridMarks 4 g) $ mem') !! x
--         memf' x = mem' !! x
--         step ps = S.unions $ S.map go'' ps
--         firstcs = go' s

reach' :: Garden -> Int -> GoEverywhere -> Int -> Int
reach' (g, s) steps go' n | even steps = fst $ memf ((div n steps) + 1)
                          | otherwise = fst $ memf' ((div n steps) + 1)
  where mem = (0, S.empty):(1, S.singleton s):map step (zip mem (tail mem))
--         memf x = (ftrace (gridMarks 4 g) $ mem) !! x
        memf x = mem !! x
        step ((_, ps'), (pl, ps)) = let cs = newcs ps ps' in (pl + S.size cs, cs)
        mem' = (0, S.empty):(1, S.singleton s):map step' (zip mem (tail mem))
--         memf' x = (ftrace (gridMarks 4 g) $ mem') !! x
        memf' x = mem' !! x
        step' ((pl', ps'), (_, ps)) = let cs = newcs ps ps' in (pl' + S.size cs, cs)
        newcs ps ps' = flip S.difference (S.union ps ps') $ S.unions $ S.map go' ps

dirs :: Grid Tile -> GridGraph
dirs g = M.fromList [(c, [d | d <- map ((,) (0, 0) . flip go c) [D, R, U, L], snd d `elem` p]) | c <- p]
  where p = plots g

dirsn :: Int -> GridGraph -> GridGraph
dirsn 1 dirmap1 = dirmap1
dirsn n dirmap1 = M.mapWithKey gon dirmapn
  where go' = goAll dirmap1
        gon c = filter ((/= c) . snd) . S.toList . S.unions . map go'
        dirmapn = dirsn (n - 1) dirmap1

part1 :: Solver
part1 input = show $ map solve [6,64]
--   where solve = rtrace $ reach g' (goAll dirmap1)
  where solve = rtrace $ reach g' (goAll dirmap1) 2 (goAll dirmap2)
        dirmap1 = dirs $ fst g'
        dirmap2 = dirsn 2 dirmap1
        g' = garden g
        g = Grid $ lines input

gridmod :: GridSize -> GCoord -> IGCoord
gridmod (mx, my) (x, y) = ((div x mx, div y my), (mod x mx, mod y my))

dirs' :: Grid Tile -> GridGraph
dirs' g = M.fromList [(c, [d | d <- map (gm . flip go c) [D, R, U, L], snd d `elem` p]) | c <- p]
  where p = plots g
        gm = gridmod $ size g

part2 :: Solver
part2 input = show $ map solve [26501365]
-- part2 input = show $ map solve [6, 10, 50, 100, 500, 1000, 5000, 26501365]
-- part2 input = show $ map solve [6, 10, 50, 100, 500, 1000]
-- part2 input = show $ map solve [10, 50, 100, 500, 1000]
-- part2 input = show $ map solve [3, 6]
--   where solve = rtrace $ reach g' (goAll dirmap1)
--   where solve = rtrace $ reach g' (goAll dirmap1) 2 (goAll dirmap2)
  where solve = rtrace $ reach' g' 55 (goAll dirmap55)
--   where solve = rtrace $ reach' g' 10 (goAll dirmap10)
--   where solve = rtrace $ reach' g' 5 (goAll dirmap5)
--   where solve = rtrace $ reach' g' 3 (goAll dirmap3)
        dirmap1 = dirs' $ fst g'
        dirmap2 = dirsn 2 dirmap1
        dirmap3 = dirsn 3 dirmap1
        dirmap5 = dirsn 5 dirmap1
        dirmap10 = dirsn 10 dirmap1
        dirmap55 = dirsn 55 dirmap1
        g' = garden g
        g = Grid $ lines input

