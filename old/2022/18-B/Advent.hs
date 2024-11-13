module Main where

import Data.Ord
import Data.List
import Data.List.Split

import Data.Graph

import Debug.Trace
idtrace x = trace (show x) x

uniq :: Eq a => [a] -> [a]
uniq = map head . group

smallest :: (Show a, Bounded a, Ord a) => [a] -> a
smallest l = smallest' maxBound l
  where smallest' m [] = m
        smallest' m (a:as) = smallest' (min m a) as

greatest :: (Show a, Bounded a, Ord a) => [a] -> a
greatest l = greatest' minBound l
  where greatest' m [] = m
        greatest' m (a:as) = greatest' (max m a) as

flatten :: Tree a -> [a]
flatten t = squish t []
  where squish (Node x ts) xs = x:Prelude.foldr squish xs ts

type Coord = (Int, Int, Int)

cx (x, _, _) = x
cy (_, y, _) = y
cz (_, _, z) = z

coord :: [Int] -> Coord
coord [x, y, z] = (x, y, z)

instance Num Coord where
  (a, b, c) + (d, e, f) = (a + d, b + e, c + f)
  negate (a, b, c) = (negate a, negate b, negate c)
  _ * _ = undefined
  fromInteger _ = undefined
  abs _ = undefined
  signum _ = undefined

type Droplet = [Coord]

droplet :: [String] -> Droplet
droplet = map (coord . map read . splitOn ",")

directions = [(1, 0, 0), (0, 1, 0), (0, 0, 1)]

neighbors :: Coord -> [Coord]
neighbors xyz = map (xyz +) $ directions ++ map negate directions

vertexes :: Coord -> (Coord, Coord, [Coord])
vertexes xyz = (xyz, xyz, neighbors xyz)

allCoords :: Coord -> Coord -> [Coord]
allCoords (fx, fy, fz) (tx, ty, tz) = [(x, y, z) | x <- [fx..tx], y <- [fy..ty], z <- [fz..tz]]

minCoord :: Droplet -> Coord
minCoord d = (smallest $ map cx d, smallest $ map cy d, smallest $ map cz d) - (1, 1, 1)

maxCoord :: Droplet -> Coord
maxCoord d = (greatest $ map cx d, greatest $ map cy d, greatest $ map cz d) + (1, 1, 1)

-- air :: Droplet -> Forest Vertex
air d = filter airHasCoord allSurfaces
  where allAir = filter (flip notElem d) $ allCoords (minCoord d) (maxCoord d)
        outsideAir = last $ sortBy (comparing length) $ components g
        allSurfaces = filter (flip notElem d) $ concat $ map neighbors d
        (g, nodeFromVertex, _) = graphFromEdges $ map vertexes allAir
        treeToGraph t = graphFromEdges $ map nodeFromVertex $ flatten t
        (airg, _, airVertexFromKey) = treeToGraph outsideAir
        airHasCoord xyz = airVertexFromKey xyz /= Nothing

surfaces :: Droplet -> Int
surfaces d = length $ air d

main = do
  cnt <- getContents
  print $ surfaces $ droplet $ lines cnt

