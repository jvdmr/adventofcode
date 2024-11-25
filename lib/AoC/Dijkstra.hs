{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts, TypeFamilies #-}

module AoC.Dijkstra
  ( dijkstra
  , GName (..)
  , GGraph (..)
  , Distance (..)
  ) where

import Data.List (sort)
import Data.Map ((!))
import qualified Data.Map as M

-- Define according to your node's id - coordinates, labels, ...
class (Eq a, Ord a) => GName a

class GGraph g where
  type GNodeName g
  nodes :: g -> [GNodeName g]
  neighbors :: g -> GNodeName g -> [GNodeName g]
  distance :: g -> GNodeName g -> GNodeName g -> Distance

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
type Explored gname = [gname]
type Queue gname = [(Distance, gname)]
type Result gname = M.Map gname (gname, Distance)
type Node gname = (gname, [(Distance, gname)])

updateResult :: GName a => Result a -> (Distance, a) -> Result a
updateResult r (d, n) | snd (r ! n) > d = M.insert n (n, d) r
                      | otherwise = r

keepFirst :: Eq b => (a -> b) -> [a] -> [a]
keepFirst f a = keepFirst' [] a
  where keepFirst' begin [] = reverse begin
        keepFirst' begin (x:rest) | notElem (f x) (map f begin) = keepFirst' (x:begin) rest
                                  | otherwise = keepFirst' begin rest

dijkstraLoop :: GName k => DGraph k (Node k) -> Explored k -> Queue k -> Result k -> Result k
dijkstraLoop _ _ [] r = r
dijkstraLoop g e ((d, n):q) r = dijkstraLoop g (n:e) (keepFirst snd $ sort (q ++ nxt)) (foldl updateResult r nxt)
  where nxt = filter unseen $ map totalDistance $ snd $ g ! n
        totalDistance (nd, nn) = (nd + d, nn)
        unseen (_, nn) = notElem nn e

dijkstra :: (GName (GNodeName g), GGraph g) => g -> GNodeName g -> Result (GNodeName g)
dijkstra graph start = dijkstraLoop nodified [] [(0, start)] $ flip updateResult (0, start) $ M.fromList $ map (\n -> (n, (n, Infinity))) $ nodes graph
  where nodified = M.fromList $ map nodify $ nodes graph
        nodify n = (n, (n, map (\n' -> (distance graph n n', n')) $ neighbors graph n))
