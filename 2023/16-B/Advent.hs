module Main where

import Data.List

import Debug.Trace
idtrace x = trace (show x) x

flatten :: [[a]] -> [a]
flatten = foldl (++) []

between :: Int -> Int -> Int -> Bool
between a b c = a <= b && b <= c

onFst :: (a -> c) -> (a, b) -> (c, b)
onFst f (a, b) = (f a, b)

onSnd :: (b -> c) -> (a, b) -> (a, c)
onSnd f (a, b) = (a, f b)

type Coord = (Int, Int)

add :: Coord -> Coord -> Coord
add (a, b) (c, d) = (a + c, b + d)

data Grid a = Grid [[a]]

instance (Show a, Eq a) => Show (Grid a) where
  show (Grid g) = flatten $ map ((++ "\n") . show) g

(!) :: Grid a -> Coord -> a
(!) (Grid g) (x, y) = (g !! y) !! x

coords :: Grid a -> [Coord]
coords (Grid g) = [(x, y) | x <- [0..mx], y <- [0..my]] 
  where my = length g - 1
        mx = length (head g) - 1

inGrid :: Grid a -> Coord -> Bool
inGrid (Grid g) (x, y) = between 0 x maxX && between 0 y maxY
  where maxX = (length $ head g) - 1
        maxY = (length g) - 1

data Direction = R | D | L | U
  deriving (Show, Eq)

type Beam = (Coord, Direction)

bfs :: (Eq a) => (a -> [a]) -> [a] -> [a] -> [a]
bfs _ result [] = result
bfs neighbors result (node:queue) | elem node result = bfs neighbors result queue
                                  | otherwise = bfs neighbors (node:result) $ queue ++ neighbors node

beam :: Direction -> Beam
beam R = ((1, 0), R)
beam L = ((-1, 0), L)
beam U = ((0, -1), U)
beam D = ((0, 1), D)

next :: Char -> Direction -> [Beam]
next '.' d = [beam d]
next '/' U = [beam R]
next '/' R = [beam U]
next '/' D = [beam L]
next '/' L = [beam D]
next '\\' U = [beam L]
next '\\' L = [beam U]
next '\\' D = [beam R]
next '\\' R = [beam D]
next '|' R = map beam [U, D]
next '|' L = map beam [U, D]
next '|' d = [beam d]
next '-' U = map beam [R, L]
next '-' D = map beam [R, L]
next '-' d = [beam d]

energize :: Grid Char -> Beam -> Int
energize g b = length $ nub $ map fst $ bfs nb [] [b]
  where nb (c, d) = filter (inGrid g . fst) $ map (onFst $ add c) $ next (g ! c) d

edge :: Grid a -> Direction -> [Beam]
edge (Grid g) D = [((x, 0), D) | x <- [0..length (head g) - 1]]
edge (Grid g) U = [((x, length g - 1), U) | x <- [0..length (head g) - 1]]
edge (Grid g) R = [((0, y), R) | y <- [0..length g - 1]]
edge (Grid g) L = [((0, length (head g) - 1), L) | y <- [0..length g - 1]]

maxEnergize :: Grid Char -> Int
maxEnergize g = foldl max 0 $ map (energize g) edges
  where edges = flatten $ map (edge g) [D, L, U, R]

main = do
  cnt <- getContents
  print $ maxEnergize $ Grid $ lines cnt

