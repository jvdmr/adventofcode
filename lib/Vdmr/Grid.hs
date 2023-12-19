{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module Vdmr.Grid
  ( (!)
  , Coord (..)
  , Direction (..)
  , Grid (..)
  , InOrOut (..)
  , add
  , border
  , coords
  , coordsG
  , countIOO
  , drawCoords
  , drawGrid
  , fromCoords
  , go
  , inGrid
  , inorout
  , inside
  , mapG
  , maxCoord
  , noborder
  , outside
  , surround
  ) where

import Vdmr.Generic

type Coord = (Int, Int)

add :: Coord -> Coord -> Coord
add (a, b) (c, d) = (a + c, b + d)

data Grid a = Grid [[a]]

drawGrid :: Grid Char -> String
drawGrid (Grid g) = flatten $ map ((++ "\n") . show) g

instance (Show a, Eq a) => Show (Grid a) where
  show (Grid g) = flatten $ map ((++ "\n") . flatten . map show) g

(!) :: Grid a -> Coord -> a
(!) (Grid g) (x, y) = (g !! y) !! x

coords :: Grid a -> [Coord]
coords (Grid g) = [(x, y) | x <- [0..mx], y <- [0..my]] 
  where my = length g - 1
        mx = length (head g) - 1

coordsG :: Grid a -> Grid Coord
coordsG (Grid g) = Grid [[(x, y) | x <- [0..mx]] | y <- [0..my]] 
  where my = length g - 1
        mx = length (head g) - 1

maxCoord :: Grid a -> Coord
maxCoord (Grid g) = (mx, my)
  where my = length g - 1
        mx = length (head g) - 1

mapG :: (a -> b) -> Grid a -> Grid b
mapG f (Grid g) = Grid $ map (map f) g

inGrid :: Grid a -> Coord -> Bool
inGrid (Grid g) (x, y) = between 0 x mx && between 0 y my
  where my = length g - 1
        mx = length (head g) - 1

surround :: Coord -> [Coord]
surround (x, y) = [(xn, yn) | xn <- map (+x) [-1, 0, 1], yn <- map (+y) [-1, 0, 1], xn /= x || yn /= y]

data Direction = D | R | U | L
  deriving (Show, Eq, Read)

go :: Direction -> Coord -> Coord
go D c = add c (0, 1)
go R c = add c (1, 0)
go U c = add c (0, -1)
go L c = add c (-1, 0)

fromCoords :: [Coord] -> Grid Coord
fromCoords cs = Grid [[(x, y) | x <- [minX..maxX]] | y <- [minY..maxY]]
  where minX = minimum $ map fst cs
        minY = minimum $ map snd cs
        maxX = maximum $ map fst cs
        maxY = maximum $ map snd cs

drawCoords :: a -> a -> [Coord] -> Grid a
drawCoords yes no cs = mapG draw $ fromCoords cs
  where draw c | elem c cs = yes
               | otherwise = no

border :: a -> Grid a -> Grid a
border a (Grid g) = Grid ([y'] ++ g' ++ [y'])
  where g' = map (([a] ++) . (++ [a])) g
        lx' = length $ head g'
        y' = take lx' $ repeat a

noborder :: Grid a -> Grid a
noborder (Grid g) = Grid (map (init . tail) $ init $ tail g)

data InOrOut = I | O
  deriving (Eq)

instance Show InOrOut where
  show I = "I"
  show O = " "

inorout :: [Coord] -> Grid a -> Grid InOrOut
inorout outC g = mapG ioo $ coordsG g
  where ioo c | elem c outC = O
              | otherwise = I

countIOO :: InOrOut -> Grid InOrOut -> Int
countIOO x (Grid g) = length $ filter (== x) $ flatten g

outside :: [Coord] -> [Coord]
outside l = filter (inGrid g) $ map (add (-1, -1)) $ bfs nb l' [(0, 0)]
  where nb c = filter (inGrid g') [go d c | d <- [D, R, U, L]]
        g = drawCoords 1 0 l
        g' = border 0 g
        l' = map (add (-mx + 1, -my + 1)) l
        mx = minimum $ map fst l
        my = minimum $ map snd l

inside :: [Coord] -> [Coord]
inside l = l ++ (filter ((== I) . (g !)) $ coords g)
  where outC = outside l
        g = drawCoords O I outC

