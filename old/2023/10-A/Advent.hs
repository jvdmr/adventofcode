module Main where

import Data.List

import Debug.Trace
idtrace x = trace (show x) x

between :: Int -> Int -> Int -> Bool
between a b c = a <= b && b <= c

unjust :: Maybe a -> a
unjust (Just a) = a
unjust Nothing = error "Nothing!"

type Coord = (Int, Int)

add :: Coord -> Coord -> Coord
add (a, b) (c, d) = (a + c, b + d)

data Grid a = Grid [[a]]

instance (Show a, Eq a) => Show (Grid a) where
  show (Grid g) = concat $ map ((++ "\n") . show) g

(!) :: Grid a -> Coord -> a
(!) (Grid g) (x, y) = (g !! y) !! x

mapG :: (a -> b) -> Grid a -> Grid b
mapG f (Grid g) = Grid $ map (map f) g

coords :: Grid a -> [Coord]
coords (Grid g) = [(x, y) | x <- [0..mx], y <- [0..my]] 
  where my = length g - 1
        mx = length (head g) - 1

surround :: Coord -> [Coord]
surround (x, y) = [(xn, yn) | xn <- map (+x) [-1, 0, 1], yn <- map (+y) [-1, 0, 1], xn /= x || yn /= y]

inGrid :: Grid a -> Coord -> Bool
inGrid (Grid g) (x, y) = between 0 x maxX && between 0 y maxY
  where maxX = (length $ head g) - 1
        maxY = (length g) - 1

type Pipe = Char

anyP :: (a -> Bool) -> (a, a) -> Bool
anyP f (a, b) = f a || f b

isPipe :: Char -> Bool
isPipe '|' = True
isPipe '-' = True
isPipe 'L' = True
isPipe 'J' = True
isPipe '7' = True
isPipe 'F' = True
isPipe 'S' = True
isPipe _ = False

nodes :: Coord -> Pipe -> (Coord, Coord)
nodes pos '|' = (add pos ( 0, -1), add pos ( 0,  1))
nodes pos '-' = (add pos (-1,  0), add pos ( 1,  0))
nodes pos 'L' = (add pos ( 0, -1), add pos ( 1,  0))
nodes pos 'J' = (add pos ( 0, -1), add pos (-1,  0))
nodes pos '7' = (add pos ( 0,  1), add pos (-1,  0))
nodes pos 'F' = (add pos ( 0,  1), add pos ( 1,  0))

nextPos :: Grid Pipe -> Coord -> Coord -> Coord
nextPos g from cur | a == from = b
                   | otherwise = a
                   where (a, b) = nodes cur (g ! cur)

start :: Grid Pipe -> Coord
start g = unjust $ find ((== 'S') . (!) g) $ coords g

startNodes :: Grid Pipe -> Coord -> [Coord]
startNodes g s = filter linked $ filter (isPipe . ((!) g)) $ filter (inGrid g) $ surround s
  where linked c = anyP (== s) $ nodes c $ g ! c

loop :: Grid Pipe -> [Coord]
loop g = s:takeWhile (/= s) (loop' s n)
  where s = start g
        n = head $ startNodes g s
        loop' prev cur = cur:loop' cur (nextPos g prev cur)

loopLength :: Grid Pipe -> Int
loopLength g = length $ loop g

main = do
  cnt <- getContents
  print $ flip (div) 2 $ loopLength $ Grid $ lines cnt

