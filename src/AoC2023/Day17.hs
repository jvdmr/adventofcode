{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts, TypeFamilies #-}
module AoC2023.Day17
  ( part1
  , part2
  ) where

import Data.List
import qualified Data.Map as M

import AoC (Solver)
import AoC.Grid hiding (Direction)
import AoC.Dijkstra

type GCoord = Coord Int

data Direction = H | V | Q
  deriving (Show, Eq, Ord)

type Block = (GCoord, Direction)

type Lines = M.Map Block Distance

type Graph = M.Map Block Lines

-- <!-- Dijkstra stuff

instance GName Block

instance GGraph Graph where
  type GNodeName Graph = Block
  edges graph n = M.toList $ graph M.! n
  nodes = M.keys

-- -->

gridNeighbors :: [Int] -> Grid Int -> Direction -> GCoord -> Lines
gridNeighbors boundaries g V (x, y) = M.fromList [(((x', y), H), Dist d) |
  x' <- map (x +) boundaries,
  d <- [sum $ map (g !) [(x'', y) | x'' <- [min x x'..max x x'], x'' /= x]],
  inGrid g (x', y)]
gridNeighbors boundaries g H (x, y) = M.fromList [(((x, y'), V), Dist d) |
  y' <- map (y +) boundaries,
  d <- [sum $ map (g !) [(x, y'') | y'' <- [min y y'..max y y'], y'' /= y]],
  inGrid g (x, y')]
gridNeighbors _ g Q (x, y) = M.fromList [(((x, y), d), Dist 0) | d <- [H, V]]

graphify :: [Int] -> Grid Int -> Graph
graphify boundaries g = M.fromList [((c, dir), gridNeighbors boundaries g dir c) | c <- coords g, dir <- [H, V]]

readInt :: Char -> Int
readInt c = read [c]

heatloss :: [Int] -> Grid Int -> Int
heatloss boundaries g = d
  where gg = M.insert ((0, 0), Q) (gridNeighbors boundaries g Q (0, 0)) $ graphify boundaries g
        dg = dijkstra gg ((0, 0), Q)
        (Dist d) = minimum $ map dg [(maxCoord g, d) | d <- [V, H]]

part1 :: Solver
part1 = show . heatloss boundaries . mapG readInt . Grid . lines
  where boundaries = reverse $ sort $ concat [[i, -i] | i <- [1..3]]

part2 :: Solver
part2 = show . heatloss boundaries . mapG readInt . Grid . lines
  where boundaries = reverse $ sort $ concat [[i, -i] | i <- [4..10]]

