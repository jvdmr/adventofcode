{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC.Grid3D
  ( (!)
  , Axis (..)
  , Coord (..)
  , Direction (..)
  , Grid (..)
  , add
  , cget
  , coords
  , coordsG
  , distance
  , drawGrid
  , fromCoords
  , go
  , inGrid
  , insertAt
  , mapG
  , maxCoord
  , neg
  , surround
  , ungrid
  ) where

import Data.List (transpose)

import AoC.Util (between)
import qualified AoC.Trace as T (showGrids, showCGrids)

data Axis = X | Y | Z
  deriving (Show, Eq)

type Coord a = (a, a, a)

cget :: Axis -> Coord a -> a
cget X (x, _, _) = x
cget Y (_, y, _) = y
cget Z (_, _, z) = z

add :: Num a => Coord a -> Coord a -> Coord a
add (a, b, c) (d, e, f) = (a + d, b + e, c + f)

neg :: Num a => Coord a -> Coord a
neg (a, b, c) = (-a, -b, -c)

distance :: Floating a => Coord a -> Coord a -> a
distance (a, b, c) (d, e, f) = sqrt $ x^2 + y^2 + z^2
  where x = abs (a - d)
        y = abs (b - e)
        z = abs (c - f)

data Grid a = Grid [[[a]]]

ungrid :: Grid a -> [[[a]]]
ungrid (Grid a) = a

-- prints grid slice by slice, z axis going up, every slice being a coordinate on the y axis
drawGrid :: Grid Char -> String
drawGrid = T.showCGrids . transpose . reverse . ungrid

instance (Show a) => Show (Grid a) where
  show = T.showGrids . transpose . reverse . ungrid

instance (Eq a) => Eq (Grid a) where
  Grid a == Grid b = a == b

(!) :: Integral b => Grid a -> Coord b -> a
(!) (Grid g) (x, y, z) = ((g !! fromIntegral z) !! fromIntegral y) !! fromIntegral x

coords :: (Enum b, Num b) => Grid a -> [Coord b]
coords (Grid g) = [(x, y, z) | x <- [0..mx], y <- [0..my], z <- [0..mz]] 
  where mz = fromIntegral $ length g - 1
        my = fromIntegral $ length (head g) - 1
        mx = fromIntegral $ length (head (head g)) - 1

coordsG :: (Enum b, Num b) => Grid a -> Grid (Coord b)
coordsG (Grid g) = Grid [[[(x, y, z) | x <- [0..mx]] | y <- [0..my]] | z <- [0..mz]]
  where mz = fromIntegral $ length g - 1
        my = fromIntegral $ length (head g) - 1
        mx = fromIntegral $ length (head (head g)) - 1

maxCoord :: Num b => Grid a -> Coord b
maxCoord (Grid g) = (mx, my, mz)
  where mz = fromIntegral $ length g - 1
        my = fromIntegral $ length (head g) - 1
        mx = fromIntegral $ length (head (head g)) - 1

mapG :: (a -> b) -> Grid a -> Grid b
mapG f (Grid g) = Grid $ map (map (map f)) g

inGrid :: (Ord b, Num b) => Grid a -> Coord b -> Bool
inGrid (Grid g) (x, y, z) = between 0 mx x && between 0 my y && between 0 mz z
  where mz = fromIntegral $ length g - 1
        my = fromIntegral $ length (head g) - 1
        mx = fromIntegral $ length (head (head g)) - 1

surround :: (Eq a, Num a) => Coord a -> [Coord a]
surround (x, y, z) = [(xn, yn, zn) | xn <- map (+x) [-1, 0, 1], yn <- map (+y) [-1, 0, 1], zn <- map (+z) [-1, 0, 1], xn /= x || yn /= y || zn /= z]

data Direction = D | R | U | L | F | B
  deriving (Show, Eq, Read)

go :: Num a => Direction -> Coord a -> Coord a
go R c = add c (1, 0, 0)
go L c = add c (-1, 0, 0)
go F c = add c (0, 1, 0)
go B c = add c (0, -1, 0)
go U c = add c (0, 0, 1)
go D c = add c (0, 0, -1)

fromCoords :: (Ord a, Enum a) => [Coord a] -> Grid (Coord a)
fromCoords cs = Grid [[[(x, y, z) | x <- [minX..maxX]] | y <- [minY..maxY]] | z <- [minZ..maxZ]]
  where minX = minimum $ map (\(x, _, _) -> x) cs
        maxX = maximum $ map (\(x, _, _) -> x) cs
        minY = minimum $ map (\(_, y, _) -> y) cs
        maxY = maximum $ map (\(_, y, _) -> y) cs
        minZ = minimum $ map (\(_, _, z) -> z) cs
        maxZ = maximum $ map (\(_, _, z) -> z) cs

insertAt :: (Integral b) => Grid a -> a -> [Coord b] -> Grid a
insertAt g v cs = mapG fv $ coordsG g
  where fv i | i `elem` cs = v
             | otherwise = g ! i

