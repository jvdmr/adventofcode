{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module Vdmr.Grid3D
  ( (!)
  , Axis (..)
  , Coord (..)
  , Direction (..)
  , Grid (..)
  , add
  , cget
  , coords
  , coordsG
  , drawCoords
  , fromCoords
  , go
  , inGrid
  , mapG
  , maxCoord
  , neg
  , surround
  ) where

import Vdmr.Generic (Solver, between)

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

data Grid a = Grid [[[a]]]

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
inGrid (Grid g) (x, y, z) = between 0 x mx && between 0 y my && between 0 z mz
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

drawCoords :: (Eq b, Ord b, Enum b) => a -> a -> [Coord b] -> Grid a
drawCoords yes no cs = mapG draw $ fromCoords cs
  where draw c | elem c cs = yes
               | otherwise = no

