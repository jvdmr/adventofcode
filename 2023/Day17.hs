{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module Day17
  ( part1
  , part2
  ) where

import Data.List
import Data.Map ((!))
import qualified Data.Map as M

import Vdmr.Generic
import Vdmr.Grid hiding ((!))
import qualified Vdmr.Grid as J ((!))
import Vdmr.Dijkstra hiding (GName)

data Direction = H | V | Q
  deriving (Show, Eq, Ord)

type Block = (Coord, Direction)

coord :: Block -> Coord
coord = fst

dir :: Block -> Direction
dir = snd

type Line = (Block, Int)
compareLine :: Line -> Line -> Ordering
compareLine (_, a) (_, b) = compare a b

type Lines = (Block, [Line])
glines :: Lines -> [Line]
glines = snd

instance GName Block

instance GGraph (Graph Block Lines) where
  distance g a b = Dist $ snd $ unjust $ find ((== coord b) . coord . fst) $ glines $ val g a
  neighbors graph n = map fst $ glines $ val graph n

instance GNode Lines where
  name (b, _) = b

gridNeighbors :: [Int] -> Grid Int -> Direction -> Coord -> [Line]
gridNeighbors boundaries g V (x, y) = sortBy compareLine [(((x', y), H), d) |
  x' <- map (x +) boundaries,
  d <- [sum $ map (g J.!) [(x'', y) | x'' <- [min x x'..max x x'], x'' /= x]],
  inGrid g (x', y)]
gridNeighbors boundaries g H (x, y) = sortBy compareLine [(((x, y'), V), d) |
  y' <- map (y +) boundaries,
  d <- [sum $ map (g J.!) [(x, y'') | y'' <- [min y y'..max y y'], y'' /= y]],
  inGrid g (x, y')]
gridNeighbors _ g Q (x, y) = [(((x, y), d), 0) | d <- [H, V]]

graphify :: [Int] -> Grid Int -> Graph Block Lines
graphify boundaries g = M.fromList [((c, dir), ((c, dir), gridNeighbors boundaries g dir c)) | c <- coords g, dir <- [H, V]]

readInt :: Char -> Int
readInt c = read [c]

heatloss :: [Int] -> Grid Int -> Int
heatloss boundaries g = d
  where gg = M.insert ((0, 0), Q) (((0, 0), Q), gridNeighbors boundaries g Q (0, 0)) $ graphify boundaries g
        dg = dijkstra gg ((0, 0), Q)
        (Dist d) = snd $ minimum $ map (val dg) [(maxCoord g, d) | d <- [V, H]]

part1 :: Solver
part1 = show . heatloss boundaries . mapG readInt . Grid . lines
  where boundaries = reverse $ sort $ flatten [[i, -i] | i <- [1..3]]

part2 :: Solver
part2 = show . heatloss boundaries . mapG readInt . Grid . lines
  where boundaries = reverse $ sort $ flatten [[i, -i] | i <- [4..10]]

