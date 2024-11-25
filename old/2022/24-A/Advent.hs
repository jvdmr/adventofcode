module Main where

-- import Data.List

import Debug.Trace
idtrace x = trace (show x) x
ftrace f x = trace (f x) x

eq :: Eq b => (a -> b) -> a -> a -> Bool
eq f a b = f a == f b

type Coord = (Int, Int)

instance Num Coord where
  (+) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
  negate (a, b) = (-a, -b)
  (*) (x1, y1) (x2, y2) = (x1 * x2, y1 * y2)
  fromInteger n = (fromInteger n, fromInteger n)
  abs (a, b) = (abs a, abs b)
  signum _ = undefined

compareYX (x1, y1) (x2, y2) = compare (y1, x1) (y2, x2)

modXY (x, y) (mx, my) = (mod x mx, mod y my)

type Direction = Coord
type Blizzard = (Direction, Coord)
type Blizzards = [Blizzard]
type Expedition = Coord

direction :: Blizzard -> Direction
direction = fst

allCoords :: Coord -> [Coord]
allCoords (lx, ly) = [(x, y) | y <- [0..ly - 1], x <- [0..lx - 1]]

loc :: [[a]] -> Coord -> a
loc m (x, y) = m !! y !! x

stay = (0, 0)
up = (0, -1)
down = (0, 1)
left = (-1, 0)
right = (1, 0)

blizzard :: Char -> Direction
blizzard '#' = stay
blizzard '.' = stay
blizzard '^' = up
blizzard 'v' = down
blizzard '<' = left
blizzard '>' = right

blizzardify :: [[Char]] -> Coord -> Blizzard
blizzardify input xy = (blizzard $ loc input xy, xy)

findBlizzards :: [[Char]] -> Blizzards
findBlizzards input = filter ((/= stay) . direction) $ map (blizzardify input) $ allCoords (lx, ly)
  where lx = length $ head input
        ly = length input

type Map = (Coord, Blizzards)

makeMap :: [String] -> Map
makeMap ls = ((mx, my), findBlizzards ls')
  where ls' = map (init . tail) $ init $ tail ls
        mx = length $ head ls'
        my = length ls'

move :: Coord -> Int -> Blizzard -> Blizzard
move bounds n (dir, xy) = (dir, modXY (xy + dir * (n, n)) bounds)

step :: Map -> Int -> [Coord]
step (bounds, blizzards) n = map (snd . move bounds n) blizzards

neighbors :: Coord -> Coord -> [Coord]
neighbors (bx, by) = loc neighbors'
  where within (x, y) = 0 <= x && x < bx && 0 <= y && y < by
        neighbors' = [[filter within (map (+ (x, y)) [up, down, left, right, stay]) | x <- [0..bx - 1]] | y <- [0..by - 1]]

relevantBlizzards :: Coord -> Blizzards -> Coord -> Blizzards
relevantBlizzards (bx, by) bzs = loc relevant
  where isRelevant (x, y) ((0, _), (bx, _)) = bx == x
        isRelevant (x, y) ((_, 0), (_, by)) = by == y
        relevant = [[filter (isRelevant (x, y)) bzs | x <- [0..bx - 1]] | y <- [0..by - 1]]

steps' :: Coord -> (Coord -> Blizzards) -> (Coord -> [Coord]) -> Coord -> ([(Int, Expedition)] -> (Int, Expedition)) -> [(Int, Expedition)] -> (Int, Expedition)
steps' bounds rbf nb end f ((n, e):rst) | end == e = (n, e)
                                        | otherwise = f (rst ++ next)
                                where noblizzard xy = notElem xy $ step (bounds, rbf xy) n'
                                      n' = n + 1
                                      next = filter (flip notElem rst) $ zip [n', n'..] $ filter noblizzard $ nb e

steps :: Map -> Int
steps (bounds, blizzards) = 1 + (fst $ f [(1, stay)])
  where end = bounds - (1, 1)
        f = steps' bounds rbf nb end f
        nb = neighbors bounds
        rbf = relevantBlizzards bounds blizzards

main = do
  cnt <- getContents
  print $ steps $ makeMap $ lines cnt
