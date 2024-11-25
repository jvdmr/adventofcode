module Main where

-- import Data.List

import Debug.Trace
idtrace x = trace (show x) x

uniq :: Eq a => [a] -> [a]
uniq = map head . group

expandY :: Int -> [[Char]] -> [Int]
expandY _ [] = []
expandY n (y:ys) | all (== '.') y = n:rest
                 | otherwise = rest
                 where rest = expandY (n + 1) ys

expandedGrid :: [[Char]] -> DGrid ([Int], [Int]) Char
expandedGrid g = Grid (xs, ys) g
  where ys = expandY 0 g
        xs = expandY 0 $ transpose g

type Coord = (Int, Int)

add :: Coord -> Coord -> Coord
add (a, b) (c, d) = (a + c, b + d)

diff :: Coord -> Coord -> Coord
diff (a, b) (c, d) = (a - c, b - d)

data DGrid a b = Grid a [[b]]

instance (Show a, Show b) => Show (DGrid a b) where
  show (Grid d g) = show d ++ "\n" ++ (concat $ map ((++ "\n") . show) g)

(!) :: DGrid a b -> Coord -> b
(!) (Grid _ g) (x, y) = (g !! y) !! x

coords :: DGrid a b -> [Coord]
coords (Grid _ g) = [(x, y) | x <- [0..mx], y <- [0..my]] 
  where my = length g - 1
        mx = length (head g) - 1

nodes :: DGrid a Char -> [Coord]
nodes g = filter ((== '#') . (!) g) $ coords g

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [_] = []
pairs (a:bs) = (map ((,) a) bs) ++ pairs bs

e :: Int
e = 999999

distance :: DGrid ([Int], [Int]) Char -> (Coord, Coord) -> Int
distance (Grid (xs, ys) _) (a@(x1, y1), b@(x2, y2)) = abs x + abs y + e * emptySpace
  where (x, y) = diff a b
        emptySpaceX = length $ takeWhile (< (max x1 x2)) $ dropWhile (< (min x1 x2)) xs
        emptySpaceY = length $ takeWhile (< (max y1 y2)) $ dropWhile (< (min y1 y2)) ys
        emptySpace = emptySpaceX + emptySpaceY

totalDistance :: DGrid ([Int], [Int]) Char -> Int
totalDistance g = sum $ map (distance g) $ pairs $ nodes g

main = do
  cnt <- getContents
  print $ totalDistance $ expandedGrid $ lines cnt

