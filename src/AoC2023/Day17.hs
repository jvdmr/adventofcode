{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts, TypeFamilies #-}
module AoC2023.Day17
  ( part1
  , part2
  ) where

import Data.List
import Data.Map ((!))
import qualified Data.Map as M

import Vdmr.Generic
import Vdmr.Grid hiding ((!), Direction)
import qualified Vdmr.Grid as J ((!))
import Vdmr.Dijkstra

type GCoord = Coord Int

data Direction = H | V | Q
  deriving (Show, Eq, Ord)

type Block = (GCoord, Direction)

coord :: Block -> GCoord
coord = fst

dir :: Block -> Direction
dir = snd

type Line = (Block, Int)
compareLine :: Line -> Line -> Ordering
compareLine (_, a) (_, b) = compare a b

type Lines = (Block, [Line])
glines :: Lines -> [Line]
glines = snd

type Graph = M.Map Block Lines

instance GName Block

instance GGraph Graph where
  type GNodeName Graph = Block
  distance graph a b = Dist $ snd $ unjust $ find ((== coord b) . coord . fst) $ glines $ graph ! a
  neighbors graph n = map fst $ glines $ graph ! n
  nodes = M.keys

gridNeighbors :: [Int] -> Grid Int -> Direction -> GCoord -> [Line]
gridNeighbors boundaries g V (x, y) = sortBy compareLine [(((x', y), H), d) |
  x' <- map (x +) boundaries,
  d <- [sum $ map (g J.!) [(x'', y) | x'' <- [min x x'..max x x'], x'' /= x]],
  inGrid g (x', y)]
gridNeighbors boundaries g H (x, y) = sortBy compareLine [(((x, y'), V), d) |
  y' <- map (y +) boundaries,
  d <- [sum $ map (g J.!) [(x, y'') | y'' <- [min y y'..max y y'], y'' /= y]],
  inGrid g (x, y')]
gridNeighbors _ g Q (x, y) = [(((x, y), d), 0) | d <- [H, V]]

graphify :: [Int] -> Grid Int -> Graph
graphify boundaries g = M.fromList [((c, dir), ((c, dir), gridNeighbors boundaries g dir c)) | c <- coords g, dir <- [H, V]]

readInt :: Char -> Int
readInt c = read [c]

heatloss :: [Int] -> Grid Int -> Int
heatloss boundaries g = d
  where gg = M.insert ((0, 0), Q) (((0, 0), Q), gridNeighbors boundaries g Q (0, 0)) $ graphify boundaries g
        dg = dijkstra gg ((0, 0), Q)
        (Dist d) = snd $ minimum $ map (dg !) [(maxCoord g, d) | d <- [V, H]]

part1 :: Solver
part1 = show . heatloss boundaries . mapG readInt . Grid . lines
  where boundaries = reverse $ sort $ concat [[i, -i] | i <- [1..3]]

part2 :: Solver
part2 = show . heatloss boundaries . mapG readInt . Grid . lines
  where boundaries = reverse $ sort $ concat [[i, -i] | i <- [4..10]]

