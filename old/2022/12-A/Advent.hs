module Main where

import Data.Char
import Data.List
import Data.List.Split

import Debug.Trace
idtrace x = trace (show x) x
ftrace f x = trace (f x) x

type Graph a = [[a]]

type Coord = (Int, Int)

val :: Show a => Graph a -> Coord -> a
val g (x, y) = (g !! x) !! y

setval g (x, y) v = (take x g) ++ [(take y rx) ++ [v] ++ (drop (y + 1) rx)] ++ (drop (x + 1) g)
  where rx = g !! x

maxxy :: Graph a -> Coord
maxxy g = (length g - 1, (length $ head g) - 1)

allcoords :: Graph a -> [Coord]
allcoords g = [(x, y) | x <- [0..maxx], y <- [0..maxy]]
  where (maxx, maxy) = maxxy g

data Distance = Dist Int
              | Infinity
              deriving (Eq, Show)

instance Ord Distance where
  compare Infinity Infinity = EQ
  compare Infinity _ = GT
  compare _ Infinity = LT
  compare (Dist a) (Dist b) = compare a b

instance Num Distance where
  negate (Dist a) = Dist $ negate a
  negate Infinity = Infinity
  (+) Infinity _ = Infinity
  (+) _ Infinity = Infinity
  (+) (Dist a) (Dist b) = Dist $ a + b
  (*) (Dist 0) _ = Dist 0
  (*) _ (Dist 0) = Dist 0
  (*) Infinity _ = Infinity
  (*) _ Infinity = Infinity
  (*) (Dist a) (Dist b) = Dist $ a * b
  fromInteger a = Dist $ fromInteger a
  abs Infinity = Infinity
  abs (Dist a) = Dist $ abs a
  signum Infinity = 1
  signum (Dist a) = Dist $ signum a

type Explored = [Coord]
type Queue = [(Distance, Coord)]
type Result = Graph Distance
type Node = (Coord, [(Distance, Coord)])

updateResult :: Result -> (Distance, Coord) -> Result
updateResult r (d, xy) | val r xy > d = setval r xy d
                       | otherwise = r

keepFirst :: Eq b => (a -> b) -> [a] -> [a]
keepFirst f a = keepFirst' [] a
  where keepFirst' begin [] = reverse begin
        keepFirst' begin (x:rest) | notElem (f x) (map f begin) = keepFirst' (x:begin) rest
                                  | otherwise = keepFirst' begin rest

dijkstraLoop :: Graph Node -> Explored -> Queue -> Result -> Result
dijkstraLoop _ _ [] r = r
dijkstraLoop g e ((d, xy):q) r = dijkstraLoop g (xy:e) (keepFirst snd $ sort (q ++ nxt)) (foldl updateResult r nxt)
  where nxt = filter unseen $ map totalDistance $ snd $ val g xy
        totalDistance (nd, nxy) = (nd + d, nxy)
        unseen (_, nxy) = notElem nxy e

dijkstra :: Show a => Graph a -> Coord -> (Graph a -> Coord -> Coord -> Distance) -> (Graph a -> Coord -> [Coord]) -> Result
dijkstra g s distance neighbors = dijkstraLoop nodified [] [(0, s)] $ map (map (\_ -> Infinity)) g
  where nodified = chunksOf (1 + snd (maxxy g)) $ map nodify $ allcoords g
        nodify n = (n, map (\xy -> (distance g n xy, xy)) $ neighbors g n)

addCoord :: Coord -> Coord -> Coord
addCoord (a, b) (c, d) = (a + c, b + d)

allneighbors g xy = filter within $ map (addCoord xy) [(-1, 0), (0, -1), (1, 0), (0, 1)]
  where within (x, y) = 0 <= x && 0 <= y && x <= maxx && y <= maxy
        (maxx, maxy) = maxxy g

height :: Graph Char -> Coord -> Char
height = val

start :: Graph Char -> Coord
start g = fst $ head $ filter ((== 'S') . snd) $ zip (allcoords g) (concat g)

end :: Graph Char -> Coord
end g = fst $ head $ filter ((== 'E') . snd) $ zip (allcoords g) (concat g)

unmarkLoc :: Char -> Char
unmarkLoc 'S' = 'a'
unmarkLoc 'E' = 'z'
unmarkLoc c = c

unmark :: Graph Char -> Graph Char
unmark g = map (map unmarkLoc) g

nxt :: Char -> Char
nxt = chr . (+ 1) . ord

allowedRange c = ['a'..nxt c]

climb :: Graph Char -> Distance
climb g = val dg (end g)
  where g' = unmark g
        distance _ _ _ = 1
        neighbors graph n = filter (reachable n) $ allneighbors graph n
        reachable n c = elem (height g' c) (allowedRange $ height g' n)
        dg = dijkstra g' (start g) distance neighbors

main = do
  cnt <- getContents
  print $ climb $ lines cnt

