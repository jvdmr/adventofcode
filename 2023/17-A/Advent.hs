module Main where

import Data.List
import Data.Map ((!))
import qualified Data.Map as M

import Debug.Trace
idtrace x = trace (show x) x

flatten :: [[a]] -> [a]
flatten = foldl (++) []

between :: Int -> Int -> Int -> Bool
between a b c = a <= b && b <= c

type Coord = (Int, Int)

add :: Coord -> Coord -> Coord
add (a, b) (c, d) = (a + c, b + d)

data Grid a = Grid [[a]]

instance (Show a, Eq a) => Show (Grid a) where
  show (Grid g) = flatten $ map ((++ "\n") . flatten . map show) g

gridval :: Grid a -> Coord -> a
gridval (Grid g) (x, y) = (g !! y) !! x

coords :: Grid a -> [Coord]
coords (Grid g) = [(x, y) | x <- [0..mx], y <- [0..my]] 
  where my = length g - 1
        mx = length (head g) - 1

end :: Grid a -> Coord
end (Grid g) = (mx, my)
  where my = length g - 1
        mx = length (head g) - 1

mapG :: (a -> b) -> Grid a -> Grid b
mapG f (Grid g) = Grid $ map (map f) g

inGrid :: Grid a -> Coord -> Bool
inGrid (Grid g) (x, y) = between 0 x maxX && between 0 y maxY
  where maxX = (length $ head g) - 1
        maxY = (length g) - 1

data Direction = H | V | Q
  deriving (Show, Eq, Ord)

type Block = (Coord, Direction)

coord :: Block -> Coord
coord = fst

dir :: Block -> Direction
dir = snd

type Line = (Block, Int)
compareLine :: Line -> Line -> Ordering
compareLine (_, a) (_, b) = compare a b

type Lines = (Block, [Line])
glines :: Lines -> [Line]
glines = snd

-- Dijkstra specific stuff starts here

-- Adjust according to your node's id - coordinates, labels, ...
type GName = Block

class GNode a where
  name :: a -> GName

class GGraph a where
  distance :: a -> GName -> GName -> Distance
  neighbors :: a -> GName -> [GName]

type Graph a = M.Map GName a

val :: GNode a => Graph a -> GName -> a
val graph n = graph ! n

setval :: GNode a => Graph a -> a -> Graph a
setval g v = M.insert (name v) v g

gmap :: (a -> b) -> Graph a -> Graph b
gmap = M.map

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

type Explored = [GName]
type Queue = [(Distance, GName)]
type Result = Graph (GName, Distance)
type Node = (GName, [(Distance, GName)])

instance GNode (GName, Distance) where
  name (n, _) = n

instance GNode Node where
  name (n, _) = n

updateResult :: Result -> (Distance, GName) -> Result
updateResult r (d, n) | snd (val r n) > d = setval r (n, d)
                      | otherwise = r

keepFirst :: Eq b => (a -> b) -> [a] -> [a]
keepFirst f a = keepFirst' [] a
  where keepFirst' begin [] = reverse begin
        keepFirst' begin (x:rest) | notElem (f x) (map f begin) = keepFirst' (x:begin) rest
                                  | otherwise = keepFirst' begin rest

dijkstraLoop :: Graph Node -> Explored -> Queue -> Result -> Result
dijkstraLoop _ _ [] r = r
dijkstraLoop g e ((d, n):q) r = dijkstraLoop g (n:e) (keepFirst snd $ sort (q ++ nxt)) (foldl updateResult r nxt)
  where nxt = filter unseen $ map totalDistance $ snd $ val g n
        totalDistance (nd, nn) = (nd + d, nn)
        unseen (_, nn) = notElem nn e

dijkstra :: (GGraph (Graph a), GNode a, Show a) => Graph a -> GName -> Result
dijkstra g s = dijkstraLoop nodified [] [(0, s)] $ flip updateResult (0, s) $ gmap (\n -> (name n, Infinity)) g
  where nodified = gmap (nodify . name) g
        nodify n = (n, map (\n' -> (distance g n n', n')) $ neighbors g n)

-- Dijkstra specific stuff ends here

fromJust (Just a) = a

instance GGraph (Graph Lines) where
  distance g a b = Dist $ snd $ fromJust $ find ((== coord b) . coord . fst) $ glines $ val g a
  neighbors graph n = map fst $ glines $ val graph n

instance GNode Lines where
  name (b, _) = b

gridNeighbors :: Grid Int -> Direction -> Coord -> [Line]
gridNeighbors g V (x, y) = sortBy compareLine [(((x', y), H), d) |
  x' <- map (x +) [3, 2, 1, -1, -2, -3],
  d <- [sum $ map (gridval g) [(x'', y) | x'' <- [min x x'..max x x'], x'' /= x]],
  inGrid g (x', y)]
gridNeighbors g H (x, y) = sortBy compareLine [(((x, y'), V), d) |
  y' <- map (y +) [3, 2, 1, -1, -2, -3],
  d <- [sum $ map (gridval g) [(x, y'') | y'' <- [min y y'..max y y'], y'' /= y]],
  inGrid g (x, y')]
gridNeighbors g Q (x, y) = [(((x, y), d), 0) | d <- [H, V]]

graphify :: Grid Int -> Graph Lines
graphify g = M.fromList [((c, dir), ((c, dir), gridNeighbors g dir c)) | c <- coords g, dir <- [H, V]]

readInt :: Char -> Int
readInt c = read [c]

heatloss :: Grid Int -> Int
heatloss g = d
  where gg = M.insert ((0, 0), Q) (((0, 0), Q), gridNeighbors g Q (0, 0)) $ graphify g
        dg = dijkstra gg ((0, 0), Q)
        (Dist d) = snd $ minimum $ map (val dg) [(end g, d) | d <- [V, H]]

main = do
  cnt <- getContents
  print $ heatloss $ mapG readInt $ Grid $ lines cnt

