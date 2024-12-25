{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts, TypeFamilies #-}

module AoC.Maze
  ( Maze (..)
  , Path (..)
  , DPath (..)
  , maze
  , findSE
  , isPath
  ) where

import AoC.Grid
import AoC.Dijkstra


-- THIS IS ALL WIP

class Grid a => Maze a where
  class Maze Location (Maze a) where
  isPath :: Location mMaze a -> l
  maze :: [String] -> Maze

class Location where
  isPath :: Grid a ->

type Path = Coord Int
type DPath = (Direction, Path)

instance Location Path where

isPath :: Grid Char -> Location -> Bool
isPath g c = '#' /= g ! c

type Maze = Grid Char

instance GName Path
instance GGraph Maze where
  type GNodeName Maze = Path
  nodes g = [(d, n) | n <- filter (isPath g) $ coords g, d <- [U, R, D, L]]
  edges g (d, c) = let turns = [((nd, c), 1000) | nd <- [clockwise d, counterclockwise d], isPath g (go nd c)]
                       turns' = [((counterclockwise d, c), 1000)]  -- special case for dead ends
                       ns = if ip then [((d, nc), 1)] else []
                       ip = isPath g nc
                       nc = go d c
                       in if null turns && not ip then turns' else turns ++ ns

findSE :: Maze -> ((Maze, Path), Location)
findSE g = ((g, (R, f 'S')), f 'E')
  where f c = head $ filter ((==) c . (!) g) $ coords g

parseInput :: [String] -> ((Maze, Path), Location)
parseInput = findSE . Grid

dijkstra1 :: Maze -> Path -> Location -> Distance
dijkstra1 m s e = head $ sort $ map (dijkstra m s) $ map (flip (,) e) [U, R, D, L]

