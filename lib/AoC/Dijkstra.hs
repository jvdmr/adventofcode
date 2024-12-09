{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts, TypeFamilies #-}

module AoC.Dijkstra
  ( dijkstra
  , dijkstraMax
  , GName (..)
  , GGraph (..)
  , Distance (..)
  ) where

import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Map ((!))
import qualified Data.Map as M

-- Define according to your node's id - coordinates, labels, ...
class (Show a, Eq a, Ord a) => GName a

-- Define according to your graph's structure
class GGraph g where
  -- GNodeName g is probably the same as GName
  type GNodeName g 
  -- get a list of all nodes
  nodes :: g -> [GNodeName g]
  -- get a list of a node's neighbors and the distance to them
  edges :: g -> GNodeName g -> [(GNodeName g, Distance)]

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

type DGraph k a = M.Map k a
type Explored gname = M.Map gname Bool
type Queue gname = [(gname, Distance)]
type Node gname = [(gname, Distance)]

dijkstraLoop :: GName k => DGraph k (Node k) -> Explored k -> Queue k -> Node k
dijkstraLoop _ _ [] = []
dijkstraLoop g e ((n, d):q) = (n, d):dijkstraLoop g e' (sortBy (comparing snd) $ filter unseen (q ++ nxt))
  where nxt = map totalDistance $ g ! n
        totalDistance (nn, nd) = (nn, nd + d)
        unseen (nn, _) = not $ M.findWithDefault False nn e'
        e' = M.insert n True e

dijkstra :: (GName (GNodeName g), GGraph g) => g -> GNodeName g -> (GNodeName g -> Distance)
dijkstra graph start = flip (M.findWithDefault Infinity) $ M.fromList $ dijkstraLoop nodified M.empty [(start, 0)]
  where nodified = M.fromList $ map nodify $ nodes graph
        nodify n = (n, edges graph n)

dijkstraMax :: (GName (GNodeName g), GGraph g) => g -> GNodeName g -> Distance -> (GNodeName g -> Bool)
dijkstraMax graph start Infinity = fix . dijkstra graph start
  where fix Infinity = False
        fix _ = True
dijkstraMax graph start maxd = flip (M.findWithDefault False) $ M.fromList $ map (flip (,) True . fst) $ takeWhile ((<= maxd) . snd) $ dijkstraLoop nodified M.empty [(start, 0)]
  where nodified = M.fromList $ map nodify $ nodes graph
        nodify n = (n, edges graph n)

