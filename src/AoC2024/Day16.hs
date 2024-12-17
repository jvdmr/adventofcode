{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts, TypeFamilies #-}
module AoC2024.Day16
  ( part1
  , part2
  , tests
  ) where

import Prelude hiding (foldl)
import Data.List (sort, sortBy, nub)
import Data.Ord (comparing)

import AoC (Solver, Tests)
import AoC.Grid
import AoC.Dijkstra

type Location = Coord Int

isPath :: Grid Char -> Location -> Bool
isPath g c = '#' /= g ! c

type Maze = Grid Char

type Path = (Direction, Location)
instance GName Path
instance GGraph Maze where
  type GNodeName Maze = Path
  nodes g = [(d, n) | n <- filter (isPath g) $ coords g, d <- [U, R, D, L]]
  edges g (d, c) = let turns = [((nd, c), 1000) | nd <- [clockwise d, counterclockwise d], isPath g (go nd c)]
                       turns' = [((counterclockwise d, c), 1000)]  -- special case for dead ends
                       ns = if ip then [((d, nc), 1)] else []
                       ip = isPath g nc
                       nc = go d c
                       in if null turns && not ip then turns' else turns ++ ns

findSE :: Maze -> ((Maze, Path), Location)
findSE g = ((g, (R, f 'S')), f 'E')
  where f c = head $ filter ((==) c . (!) g) $ coords g

parseInput :: [String] -> ((Maze, Path), Location)
parseInput = findSE . Grid

dijkstra1 :: Maze -> Path -> Location -> Distance
dijkstra1 m s e = head $ sort $ map (dijkstra m s) $ map (flip (,) e) [U, R, D, L]

tests :: Tests
tests = [show . parseInput . lines]

part1 :: Solver
part1 = show . uncurry (uncurry dijkstra1) . parseInput . lines

other :: Path -> Path
other (d, l) = (backwards d, l)

dijkstra2 :: Maze -> Path -> Location -> Int
dijkstra2 m s e | d1 e' == d2 s = length $ nub [snd n | n <- nodes m, d1 n + d2 (other n) == dist]
                | otherwise = error "FAIL"
  where d1 = dijkstra m s
        d2 = dijkstra m (other e')
        (dist, e') = head $ sortBy (comparing fst) $ map d1' es
        d1' e = (d1 e, e)
        es = map (flip (,) e) [U, R, D, L]

part2 :: Solver
part2 = show . uncurry (uncurry dijkstra2) . parseInput . lines

