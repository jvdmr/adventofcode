{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC.Grid
  ( (!)
  , Axis (..)
  , Coord (..)
  , Direction (..)
  , Grid (..)
  , InOrOut (..)
  , add
  , backwards
  , border
  , cget
  , clockwise
  , combine
  , combine3
  , coords
  , coordsG
  , coordsWhere
  , coordsWhere'
  , countIOO
  , counterclockwise
  , distance
  , drawCoords
  , drawGrid
  , drawSGrid
  , fromCoords
  , go
  , inGrid
  , inorout
  , insert
  , insertAt
  , insertAtRange
  , inside
  , mapG
  , maxCoord
  , multiply
  , neg
  , noborder
  , outside
  , parseDirection
  , setData
  , size
  , surround
  , toGrid
  , toSGrid
  , ungrid
  ) where

import AoC.Bfs (bfs)
import AoC.Pair (combine, combine3, add, neg, multiply, Pair (..))
import AoC.Util (between)
import qualified AoC.Trace as T (showGrid, showCGrid, showSGrid)

data Axis = X | Y | Z
  deriving (Show, Eq)

type Coord a = Pair a

cget :: Axis -> Coord a -> a
cget X (x, _) = x
cget Y (_, y) = y

distance :: Floating a => Coord a -> Coord a -> a
distance (a, b) (c, d) = sqrt $ x^2 + y^2
  where x = abs (a - c)
        y = abs (b - d)

data Grid a = Grid [[a]]
            | SGrid String [[a]]
  deriving (Eq)

toGrid :: Grid a -> Grid a
toGrid = Grid . ungrid

toSGrid :: String -> Grid a -> Grid a
toSGrid s = SGrid s . ungrid

setData :: Grid a -> [[b]] -> Grid b
setData (Grid _) a = Grid a
setData (SGrid s _) a = SGrid s a

ungrid :: Grid a -> [[a]]
ungrid (Grid a) = a
ungrid (SGrid _ a) = a

drawGrid :: Grid Char -> String
drawGrid = T.showCGrid . ungrid

drawSGrid :: String -> Grid String -> String
drawSGrid s = T.showSGrid s . ungrid

instance (Show a, Eq a) => Show (Grid a) where
  show (Grid a) = T.showGrid "" a
  show (SGrid s a) = T.showGrid s a

(!) :: Integral b => Grid a -> Coord b -> a
(!) g (x, y) = (ungrid g !! fromIntegral y) !! fromIntegral x

size :: (Num b) => Grid a -> Coord b
size g = (mx, my)
  where my = fromIntegral $ length $ ungrid g
        mx = fromIntegral $ length $ head $ ungrid g

maxCoord :: Num b => Grid a -> Coord b
maxCoord = add (-1, -1) . size

coords :: (Enum b, Num b) => Grid a -> [Coord b]
coords g = [(x, y) | x <- [0..mx], y <- [0..my]] 
  where (mx, my) = maxCoord g

coordsWhere :: (Integral b, Enum b, Num b) => (a -> Bool) -> Grid a -> [Coord b]
coordsWhere f g = coordsWhere' (f . (!) g) g

coordsWhere' :: (Integral b, Enum b, Num b) => (Coord b -> Bool) -> Grid a -> [Coord b]
coordsWhere' f g = filter f $ coords g

coordsG :: (Enum b, Num b) => Grid a -> Grid (Coord b)
coordsG g = setData g [[(x, y) | x <- [0..mx]] | y <- [0..my]] 
  where (mx, my) = maxCoord g

mapG :: (a -> b) -> Grid a -> Grid b
mapG f g = setData g $ map (map f) $ ungrid g

inGrid :: (Ord b, Num b) => Grid a -> Coord b -> Bool
inGrid g (x, y) = between 0 mx x && between 0 my y
  where (mx, my) = maxCoord g

surround :: (Eq a, Num a) => Coord a -> [Coord a]
surround (x, y) = [(xn, yn) | xn <- map (+x) [-1, 0, 1], yn <- map (+y) [-1, 0, 1], xn /= x || yn /= y]

data Direction = U | R | D | L
  deriving (Eq, Ord, Read, Enum)

instance Show Direction where
  show U = "^"
  show R = ">"
  show D = "v"
  show L = "<"

parseDirection :: Char -> Direction
parseDirection '^' = U
parseDirection '>' = R
parseDirection 'v' = D
parseDirection '<' = L

clockwise :: Direction -> Direction
clockwise L = U
clockwise d = succ d

counterclockwise :: Direction -> Direction
counterclockwise U = L
counterclockwise d = pred d

backwards :: Direction -> Direction
backwards = clockwise . clockwise

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
border a g = setData g ([y'] ++ g' ++ [y'])
  where g' = map (([a] ++) . (++ [a])) $ ungrid g
        lx' = length $ head g'
        y' = take lx' $ repeat a

noborder :: Grid a -> Grid a
noborder g = setData g (map (init . tail) $ init $ tail $ ungrid g)

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
countIOO x g = length $ filter (== x) $ concat $ ungrid g

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

insert :: Grid a -> a -> Coord Int -> Grid a
insert g v c = insertAtRange g v c c

insertAt :: Grid a -> a -> [Coord Int] -> Grid a
insertAt g v cs = foldl fv g cs
  where fv gr c = insertAtRange gr v c c

insertAtRange :: Grid a -> a -> Coord Int -> Coord Int -> Grid a
insertAtRange g v (xa, ya) (xb, yb) = setData g $ heady ++ [headx xs ++ middlex ++ lastx xs | xs <- middley] ++ lasty
  where heady = take ya d
        middley = drop ya $ take (yb + 1) d
        lasty = drop (yb + 1) d
        headx xs = take xa xs
        middlex = drop xa $ take (xb + 1) $ repeat v
        lastx xs = drop (xb + 1) xs
        d = ungrid g

