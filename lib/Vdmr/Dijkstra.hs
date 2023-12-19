{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}

module Vdmr.Dijkstra
  ( dijkstra
  , GName(..)
  , GGraph(..)
  , Graph
  , GNode(..)
  , Distance(..)
  , val
  ) where

import Data.List (sort)
import Data.Map ((!))
import qualified Data.Map as M

-- Define according to your node's id - coordinates, labels, ...
class GName a

class GNode a where
  name :: (GName b) => a -> b

class GGraph a where
  distance :: (GName b) => a -> b -> b -> Distance
  neighbors :: (GName b) => a -> b -> [b]

type Graph k a = M.Map k a

val :: (GName k, GNode a) => Graph k a -> k -> a
val graph n = graph ! n

setval :: (GName k, GNode a) => Graph k a -> a -> Graph k a
setval g v = M.insert (name v) v g

gmap :: (GName k) => (a -> b) -> Graph k a -> Graph k b
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
type Result = Graph GName (GName, Distance)
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

dijkstraLoop :: (GName k) => Graph k Node -> Explored -> Queue -> Result -> Result
dijkstraLoop _ _ [] r = r
dijkstraLoop g e ((d, n):q) r = dijkstraLoop g (n:e) (keepFirst snd $ sort (q ++ nxt)) (foldl updateResult r nxt)
  where nxt = filter unseen $ map totalDistance $ snd $ val g n
        totalDistance (nd, nn) = (nd + d, nn)
        unseen (_, nn) = notElem nn e

dijkstra :: (GName k, GGraph (Graph k a), GNode a, Show a) => Graph k a -> k -> Result
dijkstra g s = dijkstraLoop nodified [] [(0, s)] $ flip updateResult (0, s) $ gmap (\n -> (name n, Infinity)) g
  where nodified = gmap (nodify . name) g
        nodify n = (n, map (\n' -> (distance g n n', n')) $ neighbors g n)
