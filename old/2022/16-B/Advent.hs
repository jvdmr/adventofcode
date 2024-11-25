module Main where

import Data.Ord (comparing)
-- import Data.List
import Data.List.Split
import Data.Map ((!))
import qualified Data.Map as M

import Debug.Trace
idtrace x = trace (show x) x
ftrace f x = trace (show $ f x) x

fromJust (Just a) = a

splitBy :: (a -> Bool) -> [a] -> ([a], [a])
splitBy f l = (concat $ map fst result, concat $ map snd result)
  where result = splitBy' l
        splitBy' [] = [([], [])]
        splitBy' (a:rst) | f a = ([a], []):splitBy' rst
                         | otherwise = ([], [a]):splitBy' rst

data ValveState = Closed
                | Open Int
                deriving (Eq, Show, Ord)

data Valve = Valve String Int ValveState [(Int, String)]
  deriving (Eq, Show, Ord)

instance GNode Valve where
  name (Valve n _ _ _) = n

rate (Valve _ r _ _) = r
state (Valve _ _ s _) = s
tunnels (Valve _ _ _ t) = t

isOpen (Valve _ _ (Open _) _) = True
isOpen (Valve _ 0 _ _) = True
isOpen _ = False

open :: Int -> Valve -> Valve
open time (Valve name rate _ tunnels) = Valve name rate (Open time) tunnels

pressureReleased :: Valve -> Int
pressureReleased (Valve _ _ Closed _) = 0
pressureReleased (Valve _ rate (Open t) _) = t * rate

parseValve l = (name, Valve name rate Closed tunnels)
  where name = head $ tail valveDetails
        rate = read $ last $ splitOn "=" $ last valveDetails
        valveDetails = words valveDetailsPart
        tunnels = zip [1, 1..] $ splitOn "," $ concat $ drop 4 $ words tunnelDetailsPart
        [valveDetailsPart, tunnelDetailsPart] = splitOn "; " l

-- Dijkstra specific stuff starts here

-- Adjust according to your node's id - coordinates, labels, ...
type GName = String

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

instance GGraph (Graph Valve) where
  distance g a b = Dist $ fst $ fromJust $ find ((== b) . snd) $ tunnels $ val g a
  neighbors graph n = map snd $ tunnels $ val graph n

relax :: Graph Valve -> Graph Valve
relax g = gmap relaxNode g
  where relaxNode (Valve n r s t) = Valve n r s $ filter ((/= 0) . rate . val g . snd) $ map tunnelFromResult $ M.elems $ M.filter ((/= n) . name) $ dijkstra g n
        tunnelFromResult (n, Dist d) = (d, n)

noGoingBack :: GName -> Graph Valve -> Graph Valve
noGoingBack n g = foldl setval g $ map removeWayBack $ M.elems g
  where removeWayBack (Valve n r s t) = Valve n r s $ filter ((/= n) . snd) t

releasePressure' :: (Int, String, Graph Valve) -> [(Int, String, Graph Valve)]
releasePressure' (time, startNode, g) = filter opened $ map nextOption nextNodes
  where dg = tunnels $ val g startNode
        nextNodes = filter (not . isOpen . val g . snd) dg
        g' = noGoingBack startNode g
        opened (0, _, _) = False
        opened _ = True
        nextOption (dist, nextNode) | time - dist - 1 < 1 = (0, nextNode, g)
                                    | otherwise = (time - dist - 1, nextNode, setval g' $ open (time - dist - 1) $ val g' nextNode)

releasePressure :: [(Int, String, Graph Valve)] -> [(Int, String, Graph Valve)]
releasePressure [] = []
releasePressure nextOptions = nextOptions ++ (releasePressure $ concat $ map releasePressure' nextOptions')
  where nextOptions' = filter (\(time, _, _) -> time > 2) nextOptions

matching :: (Eq a, GNode a) => [[a]] -> Bool
matching [ga, gb] = intersect (map name ga) (map name gb) == []

works :: Valve -> Bool
works (Valve _ 0 _ _) = False
works (Valve _ _ _ _) = True

pairWith :: (a -> b) -> (a -> c) -> [a] -> [(b, c)]
pairWith fb fc a = zip (map fb a) (map fc a)

info (Valve n _ s _) = (n, s)
valveTrace = ftrace info
valvesTrace = ftrace (map info)
resultTrace = ftrace (map info . snd)

bothReleasePressure :: Int -> String -> Graph Valve -> Int
bothReleasePressure time startNode g = fst $ resultTrace $ last $ sortBy (comparing fst) $ combinations
  where results = releasePressure [(time, startNode, g)]
        graph (_, _, a) = filter isOpen $ filter works $ M.elems a
        results' = pairWith totalPressureReleased (reverse . sortBy (comparing state)) $ filter (closeEnough half . length) $ map graph results
        combinations = [(me + elephant, valvesTrace mpath ++ valvesTrace epath) | ((me, mpath):rest) <- tails results', closeEnough half $ length mpath, (elephant, epath) <- rest, closeEnough (length mpath) (length epath), matching [mpath, epath]]
        workingNames = length $ filter works $ M.elems g
        half = div workingNames 2
        closeEnough a b = abs (a - b) <= 1

totalPressureReleased :: [Valve] -> Int
totalPressureReleased = sum . map pressureReleased

main = do
  cnt <- getContents
  print $ bothReleasePressure 26 "AA" $ relax $ M.fromList $ map parseValve $ lines cnt

