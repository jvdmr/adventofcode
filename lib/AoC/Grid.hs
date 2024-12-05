{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC.Grid
  ( (!)
  , Axis (..)
  , Coord (..)
  , Direction (..)
  , Grid (..)
  , InOrOut (..)
  , add
  , border
  , cget
  , coords
  , coordsG
  , countIOO
  , drawCoords
  , drawGrid
  , fromCoords
  , go
  , inGrid
  , inorout
  , insertAt
  , inside
  , mapG
  , maxCoord
  , neg
  , noborder
  , outside
  , size
  , surround
  , ungrid
  ) where

import AoC.Bfs (bfs)
import AoC.Util (between)
import qualified AoC.Trace as T (showGrid, showCGrid)

data Axis = X | Y | Z
  deriving (Show, Eq)

type Coord a = (a, a)

cget :: Axis -> Coord a -> a
cget X (x, _) = x
cget Y (_, y) = y

add :: Num a => Coord a -> Coord a -> Coord a
add (a, b) (c, d) = (a + c, b + d)

neg :: Num a => Coord a -> Coord a
neg (a, b) = (-a, -b)

data Grid a = Grid [[a]]
  deriving (Eq)

ungrid :: Grid a -> [[a]]
ungrid (Grid a) = a

drawGrid :: Grid Char -> String
drawGrid = T.showCGrid . ungrid

instance (Show a, Eq a) => Show (Grid a) where
  show = T.showGrid . ungrid

(!) :: Integral b => Grid a -> Coord b -> a
(!) (Grid g) (x, y) = (g !! fromIntegral y) !! fromIntegral x

size :: (Num b) => Grid a -> Coord b
size (Grid g) = (mx, my)
  where my = fromIntegral $ length g
        mx = fromIntegral $ length (head g)

maxCoord :: Num b => Grid a -> Coord b
maxCoord = add (-1, -1) . size

coords :: (Enum b, Num b) => Grid a -> [Coord b]
coords g = [(x, y) | x <- [0..mx], y <- [0..my]] 
  where (mx, my) = maxCoord g

coordsG :: (Enum b, Num b) => Grid a -> Grid (Coord b)
coordsG g = Grid [[(x, y) | x <- [0..mx]] | y <- [0..my]] 
  where (mx, my) = maxCoord g

mapG :: (a -> b) -> Grid a -> Grid b
mapG f (Grid g) = Grid $ map (map f) g

inGrid :: (Ord b, Num b) => Grid a -> Coord b -> Bool
inGrid g (x, y) = between 0 mx x && between 0 my y
  where (mx, my) = maxCoord g

surround :: (Eq a, Num a) => Coord a -> [Coord a]
surround (x, y) = [(xn, yn) | xn <- map (+x) [-1, 0, 1], yn <- map (+y) [-1, 0, 1], xn /= x || yn /= y]

data Direction = D | R | U | L
  deriving (Show, Eq, Read)

go :: Num a => Direction -> Coord a -> Coord a
go D c = add c (0, 1)
go R c = add c (1, 0)
go U c = add c (0, -1)
go L c = add c (-1, 0)

fromCoords :: (Ord a, Enum a) => [Coord a] -> Grid (Coord a)
fromCoords cs = Grid [[(x, y) | x <- [minX..maxX]] | y <- [minY..maxY]]
  where minX = minimum $ map fst cs
        minY = minimum $ map snd cs
        maxX = maximum $ map fst cs
        maxY = maximum $ map snd cs

drawCoords :: (Eq b, Ord b, Enum b) => a -> a -> [Coord b] -> Grid a
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

inorout :: (Enum b, Eq b, Num b) => [Coord b] -> Grid a -> Grid InOrOut
inorout outC g = mapG ioo $ coordsG g
  where ioo c | elem c outC = O
              | otherwise = I

countIOO :: InOrOut -> Grid InOrOut -> Int
countIOO x (Grid g) = length $ filter (== x) $ concat g

outside :: (Ord a, Eq a, Num a, Enum a) => [Coord a] -> [Coord a]
outside l = [c | c <- map (add (-1, -1) . fst) $ bfs nb (0, 0), inGrid g c]
  where nb c = [c' | c' <- map (flip go c) [D, R, U, L], inGrid g' c', notElem c' l']
        g = drawCoords 1 0 l
        g' = border 0 g
        l' = map (add (-mx + 1, -my + 1)) l
        mx = minimum $ map fst l
        my = minimum $ map snd l

inside :: (Ord a, Eq a, Num a, Enum a, Integral a) => [Coord a] -> [Coord a]
inside l = l ++ (filter ((== I) . (g !)) $ coords g)
  where outC = outside l
        g = drawCoords O I outC

insertAt :: (Integral b) => Grid a -> a -> [Coord b] -> Grid a
insertAt g v cs = mapG fv $ coordsG g
  where fv i | i `elem` cs = v
             | otherwise = g ! i

