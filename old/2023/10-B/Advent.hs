module Main where

import Data.List

import Debug.Trace
idtrace x = trace (show x) x
ftrace f x = trace (show $ f x) x

between :: Int -> Int -> Int -> Bool
between a b c = a <= b && b <= c

unjust :: Maybe a -> a
unjust (Just a) = a
unjust Nothing = error "Nothing!"

type Coord = (Int, Int)

add :: Coord -> Coord -> Coord
add (a, b) (c, d) = (a + c, b + d)

data Grid a = Grid [[a]]

instance (Show a) => Show (Grid a) where
  show (Grid g) = concat $ map ((++ "\n") . concat . map show) g

(!) :: Grid a -> Coord -> a
(!) (Grid g) (x, y) = (g !! y) !! x

mapG :: (a -> b) -> Grid a -> Grid b
mapG f (Grid g) = Grid $ map (map f) g

coordsG :: Grid a -> Grid Coord
coordsG (Grid g) = Grid [[(x, y) | x <- [0..mx]] | y <- [0..my]] 
  where my = length g - 1
        mx = length (head g) - 1

coords :: Grid a -> [Coord]
coords (Grid g) = [(x, y) | x <- [0..mx], y <- [0..my]] 
  where my = length g - 1
        mx = length (head g) - 1

surround :: Coord -> [Coord]
surround (x, y) = [(xn, yn) | xn <- map (+x) [-1, 0, 1], yn <- map (+y) [-1, 0, 1], xn /= x || yn /= y]

carthesian :: Coord -> [Coord]
carthesian c = map (add c) [(-2, 0), (0, -2), (2, 0), (0, 2)]

inGrid :: Grid a -> Coord -> Bool
inGrid (Grid g) (x, y) = between 0 x maxX && between 0 y maxY
  where maxX = length $ head g
        maxY = length g

type Pipe = Char

isPipe :: Char -> Bool
isPipe '|' = True
isPipe '-' = True
isPipe 'L' = True
isPipe 'J' = True
isPipe '7' = True
isPipe 'F' = True
isPipe 'S' = True
isPipe _ = False

nodes :: Coord -> Pipe -> [Coord]
nodes pos '|' = [add pos ( 0, -1), add pos ( 0,  1)]
nodes pos '-' = [add pos (-1,  0), add pos ( 1,  0)]
nodes pos 'L' = [add pos ( 0, -1), add pos ( 1,  0)]
nodes pos 'J' = [add pos ( 0, -1), add pos (-1,  0)]
nodes pos '7' = [add pos ( 0,  1), add pos (-1,  0)]
nodes pos 'F' = [add pos ( 0,  1), add pos ( 1,  0)]

nextPos :: Grid Pipe -> Coord -> Coord -> Coord
nextPos g from cur | a == from = b
                   | otherwise = a
                   where [a, b] = nodes cur (g ! cur)

start :: Grid Pipe -> Coord
start g = unjust $ find ((== 'S') . (!) g) $ coords g

startNode :: Grid Pipe -> Coord -> Coord
startNode g s = ns $ head $ filter linked $ filter (isPipe . ((!) g)) $ filter (inGrid g) $ carthesian s
  where linked c = any (== s) $ map (nextPos g c) $ nodes c $ g ! c
        ns c = unjust $ find ((== s) . nextPos g c) $ nodes c $ g ! c

loop :: Grid Pipe -> [Coord]
loop g = s:takeWhile (/= s) (loop' s n)
  where s = start g
        n = startNode g s
        loop' prev cur = cur:loop' cur (nextPos g prev cur)

enlarge :: Grid Pipe -> Grid Pipe
enlarge (Grid g) = Grid $ enlarge'' $ map enlarge' g
  where enlarge' [] = []
        enlarge' (x:xs) = '-':x:enlarge' xs
        enlarge'' [] = []
        enlarge'' (y:ys) = y':y:enlarge'' ys
        y' = take ly' $ repeat '|'
        ly' = 2 * (length $ head g)

shrink :: Grid a -> Grid a
shrink (Grid g) = Grid $ shrink' $ map shrink' g
  where shrink' [] = []
        shrink' (_:x:xs) = x:shrink' xs

bfs :: (Eq a) => (a -> [a]) -> [a] -> [a] -> [a]
bfs _ result [] = result
bfs neighbors result (node:queue) | elem node result = bfs neighbors result queue
                                  | otherwise = bfs neighbors (node:result) $ queue ++ neighbors node

data InOrOut = I | O
  deriving (Eq)

instance Show InOrOut where
  show I = "I"
  show O = " "

inorout :: [Coord] -> Grid Pipe -> Grid InOrOut
inorout outC g = mapG ioo $ coordsG g
  where ioo c | elem c outC = O
              | otherwise = I

countIOO :: InOrOut -> Grid InOrOut -> Int
countIOO x (Grid g) = length $ filter (== x) $ concat g

outside :: Grid Pipe -> [Coord]
outside g = bfs nb [] [(0, 0)]
  where nb c | elem c l = []
             | otherwise = filter (inGrid g) $ surround c
        l = loop g

inside :: Grid Pipe -> Int
inside g = countIOO I $ idtrace $ shrink $ inorout outC g'
  where outC = outside g'
        g' = enlarge g

main = do
  cnt <- getContents
  print $ inside $ Grid $ lines cnt

