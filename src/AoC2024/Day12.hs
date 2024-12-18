{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC2024.Day12
  ( part1
  , part2
  , tests
  ) where

import AoC (Solver, Tests)
import AoC.Grid
import AoC.Bfs (bfsDisconnected)

regions :: Grid Char -> [[(Coord Int, Int)]]
regions g = bfsDisconnected neighbors $ coords g
  where neighbors c = let cv = g ! c in filter ((== cv) . (g !)) $ filter (inGrid g) $ map (flip go c) [U, R, D, L]

fencePrice :: [(Coord Int, Int)] -> Int
fencePrice cs = length cs * length nns
  where nns = concat $ map nonneighbors cs'
        nonneighbors c = filter (flip notElem cs') $ map (flip go c) [U, R, D, L]
        cs' = map fst cs

tests :: Tests
tests =
  [ show . length . lines
  ]

part1 :: Solver
part1 = show . sum . map fencePrice . regions . Grid . lines

bulkFencePrice :: [(Coord Int, Int)] -> Int
bulkFencePrice cs = length cs * length fence
  where cs' = map fst cs
        nns = concat $ map nonneighbors cs'
        nonneighbors c = map (:[c]) $ filter (flip notElem cs') $ map (flip go c) [U, R, D, L]
        fence = map (map fst) $ bfsDisconnected (fenceNeighbors nns) nns
        fenceNeighbors nns c = filter (flip elem nns) [map (go d) c | d <- [U, R, D, L]]

part2 :: Solver
part2 = show . sum . map bulkFencePrice . regions . Grid . lines

