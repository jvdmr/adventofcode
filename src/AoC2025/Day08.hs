{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC2025.Day08
  ( part1
  , part2
  , tests
  ) where

import Prelude hiding (foldl) -- use foldl' instead of foldl
import Data.List (sort, sortOn)
import Data.List.Split (splitOn)

import AoC (Solver, Tests)
import AoC.Grid3D (Coord(..), distance)
import AoC.Pair (Pair(..))
import AoC.Util (multiply, filterFirst, equating, combinationsWith)

type JunctionBox f = Coord f

junctionBox :: Read f => String -> JunctionBox f
junctionBox s = (x, y, z)
  where [x, y, z] = map read $ splitOn "," s

type Connection f = Pair (JunctionBox f)

distances :: (Floating f, Eq f) => [JunctionBox f] -> [(Connection f, f)]
distances = combinationsWith distance'
  where distance' a b = ((a, b), distance a b)

joinCircuits' :: Eq f => (Maybe (Connection f), [[JunctionBox f]]) -> [Connection f] -> (Maybe (Connection f), [[JunctionBox f]])
joinCircuits' previousResult [] = previousResult
joinCircuits' previousResult@(_, [_]) _ = previousResult
joinCircuits' previousResult@(_, circuits) ((a, b):cons) | elem b circuitA = joinCircuits' previousResult cons
                                                         | otherwise = joinCircuits' (Just (a, b), (concat circuitsAB):circuits') cons
  where circuitA = head $ filter (elem a) circuits
        circuitB = head $ filter (elem b) circuits
        circuitsAB = [circuitA, circuitB]
        circuits' = foldl' (flip filterFirst) circuits $ map (equating head) circuitsAB

joinCircuits :: Eq f => [JunctionBox f] -> [Connection f] -> (Maybe (Connection f), [[JunctionBox f]])
joinCircuits jbs = joinCircuits' (Nothing, map (:[]) jbs)

connections :: (Floating f, Ord f) => [JunctionBox f] -> [Connection f]
connections = map fst . sortOn snd . distances

numberOfConnectionsToMake :: [a] -> Int
numberOfConnectionsToMake input | length input > 50 = 1000
                                | otherwise = 10

findCircuits :: (Floating f, Ord f) => [JunctionBox f] -> [[JunctionBox f]]
findCircuits jbs = snd $ joinCircuits jbs $ take n $ connections jbs
  where n = numberOfConnectionsToMake jbs

result1 :: [[a]] -> Int
result1 = multiply . take 3 . reverse . sort . map length

part1 :: Solver
part1 = show . result1 . findCircuits . map junctionBox . lines

findSingleCircuit :: (Floating f, Ord f) => [JunctionBox f] -> Maybe (Connection f)
findSingleCircuit jbs = fst $ joinCircuits jbs $ connections jbs

result2 :: RealFrac f => Maybe (Connection f) -> Int
result2 Nothing = 0
result2 (Just ((x1, _, _), (x2, _, _))) = round $ x1 * x2

part2 :: Solver
part2 = show . result2 . findSingleCircuit . map junctionBox . lines

tests :: Tests
tests =
  [ show . length . lines
  ]

