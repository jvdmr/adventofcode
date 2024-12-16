{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts, TypeFamilies #-}
module AoC2024.Day16
  ( part1
  , part2
  , tests
  ) where

import Prelude hiding (foldl)
import Data.List (sort)

import AoC (Solver, Tests)
import AoC.Grid
import AoC.Dijkstra
import AoC.Trace

type Location = Coord Int

isPath :: Grid Char -> Location -> Bool
isPath g c = '#' /= g ! c

type Maze = Grid Char

distance :: Direction -> Direction -> Distance
distance d nd | d == nd = Dist 0
              | d == backwards nd = Dist 2000 -- 2 90° turns
              | otherwise = Dist 1000

type Path = (Direction, Location)
instance GName Path
instance GGraph Maze where
  type GNodeName Maze = Path
  nodes g = [(d, n) | n <- filter (isPath g) $ coords g, d <- [U, R, D, L]]
  edges g (d, c) = let dist = distance d in filter (isPath g . snd . fst) [((nd, go nd c), 1 + dist nd) | nd <- [U, R, D, L]]

findSE :: Maze -> (Path, Path)
findSE g = ((R, f 'S'), (A, f 'E'))
  where f c = head $ filter ((==) c . (!) g) $ coords g

parseInput :: [String] -> (Maze, (Path, Path))
parseInput ss = let g = Grid ss in (g, findSE g)

dijkstra' :: (Maze, (Path, Path)) -> Distance
dijkstra' (m, (s, e)) | fst e == A = head $ sort $ map (dijkstra m s) $ map (flip (,) $ snd e) [U, R, D, L]
                      | otherwise = dijkstra m s e

tests :: Tests
tests = [show . parseInput . lines]

part1 :: Solver
part1 = show . dijkstra' . parseInput . lines

part2 :: Solver
part2 = ("Not yet solved! " ++) . show . length . lines

