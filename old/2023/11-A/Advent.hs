module Main where

-- import Data.List

import Debug.Trace
idtrace x = trace (show x) x

rotateCCW :: [[a]] -> [[a]]
rotateCCW = transpose . map reverse

type AstronomyData = [[Char]]

expandY :: AstronomyData -> AstronomyData
expandY [] = []
expandY (y:ys) | all (== '.') y = y:y:expandY ys
               | otherwise = y:expandY ys

expand :: AstronomyData -> AstronomyData
expand = expandY . rotateCCW . expandY

type Coord = (Int, Int)

add :: Coord -> Coord -> Coord
add (a, b) (c, d) = (a + c, b + d)

diff :: Coord -> Coord -> Coord
diff (a, b) (c, d) = (a - c, b - d)

data Grid a = Grid [[a]]

instance (Show a) => Show (Grid a) where
  show (Grid g) = concat $ map ((++ "\n") . concat . map show) g

(!) :: Grid a -> Coord -> a
(!) (Grid g) (x, y) = (g !! y) !! x

coords :: Grid a -> [Coord]
coords (Grid g) = [(x, y) | x <- [0..mx], y <- [0..my]] 
  where my = length g - 1
        mx = length (head g) - 1

nodes :: Grid Char -> [Coord]
nodes g = filter ((== '#') . (!) g) $ coords g

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [_] = []
pairs (a:bs) = (map ((,) a) bs) ++ pairs bs

distance :: (Coord, Coord) -> Int
distance (a, b) = abs x + abs y
  where (x, y) = diff a b

main = do
  cnt <- getContents
  print $ sum $ map distance $ pairs $ nodes $ Grid $ expand $ lines cnt

