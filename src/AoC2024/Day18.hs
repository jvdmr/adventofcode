{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts, TypeFamilies #-}
module AoC2024.Day18
  ( part1
  , part2
  , tests
  ) where

import Prelude hiding (foldl)
import Data.List.Split (splitOn)

import AoC (Solver, Tests)
import AoC.Util (pair)
import AoC.Grid
import AoC.Dijkstra

type Location = Coord Int
type Byte = Location

parseInput :: [String] -> (Int, [Byte])
parseInput (p1n:bytess) = (read p1n, map (pair . map read . splitOn ",") bytess)

type Memory = Grid Bool -- is (x, y) still available?
instance GName Location
instance GGraph Memory where
  type GNodeName Memory = Location
  nodes g = filter ((!) g) $ coords g
  edges g c = map (flip (,) $ Dist 1) $ filter ((!) g) $ filter (inGrid g) $ map (flip go c) [U, R, D, L]

memory :: (Int, [Byte]) -> Memory
memory = drawCoords False True . uncurry take

dijkstra1' :: Memory -> Distance
dijkstra1' m = dijkstra m (0, 0) $ maxCoord m

dijkstra1 :: (Int, [Byte]) -> Distance
dijkstra1 input = dijkstra1' $ memory input

tests :: Tests
tests =
  [ show . length . lines
  , show . drawCoords False True . uncurry take . parseInput . lines
  ]

part1 :: Solver
part1 = show . dijkstra1 . parseInput . lines

dijkstra2' :: [(Byte, Memory)] -> Byte
dijkstra2' bs | null path = fst $ head bs
              | otherwise = dijkstra2' $ dropWhile (flip notElem path . fst) bs
  where path = dijkstraPaths m (0, 0) $ maxCoord m
        m = snd $ head bs

dijkstra2 :: (Int, [Byte]) -> String
dijkstra2 input = out $ dijkstra2' $ zip rst $ tail $ scanl (flip insert False) m rst
  where m = memory input
        rst = uncurry drop input
        out (x, y) = show x ++ "," ++ show y

part2 :: Solver
part2 = dijkstra2 . parseInput . lines

