{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC2025.Day09
  ( part1
  , part2
  , tests
  ) where

import Prelude hiding (foldl) -- use foldl' instead of foldl
import Data.List (sort)
import Data.Range (Range, (*=*), inRange, rangesOverlap)

import AoC (Solver, Tests)
import AoC.Grid (Coord)
import AoC.Pair (pair, Pair)
import AoC.Text (csl)
import AoC.Util (combinations, combinationsWith, zipTailWith, none)

type Tile = Coord Int

tile :: String -> Tile
tile = pair . map read . csl

area :: Tile -> Tile -> Int
area (a, b) (c, d) = (1 + abs (a - c)) * (1 + abs (b - d))

areas :: [Tile] -> [Int]
areas = combinationsWith area

part1 :: Solver
part1 = show . last . sort . areas . map tile . lines

data TileRange = Horizontal (Range Int, Int)
               | Vertical (Int, Range Int)
  deriving (Show, Eq)

crosses :: TileRange -> TileRange -> Bool
crosses (Horizontal (xra, _)) (Horizontal (xrb, _)) = rangesOverlap xra xrb
crosses (Vertical (_, yra)) (Vertical (_, yrb)) = rangesOverlap yra yrb
crosses a@(Horizontal _) b@(Vertical _) = crosses b a
crosses (Vertical (x, yr)) (Horizontal (xr, y)) = inRange xr x || inRange yr y

contains :: [TileRange] -> TileRange -> Bool
contains trs tr = all (crosses tr) trs

closeLoop :: [a] -> [a]
closeLoop lst = lst ++ [head lst]

tilerange :: Tile -> Tile -> TileRange
tilerange (a, b) (c, d) | a == c && b < d = Vertical (a, b *=* d)
                        | a == c && b > d = Vertical (a, d *=* b)
                        | a < c && b == d = Horizontal (a *=* c, b)
                        | a > c && b == d = Horizontal (c *=* a, b)

rectangle :: Tile -> Tile -> [TileRange]
rectangle (a, b) (c, d) | a == c || b == d = []  -- line
                        | otherwise = zipTailWith tilerange [(a, b), (a, d), (c, d), (c, b), (a, b)]

inArea :: [Tile] -> (Pair Tile) -> Bool
inArea tiles = fits . uncurry rectangle
  where fits rect = none (contains rect) shape
        shape = zipTailWith tilerange $ closeLoop tiles

rectangles :: [Tile] -> [Pair Tile]
rectangles = combinations

rectanglesInArea :: [Tile] -> [Pair Tile]
rectanglesInArea tiles = filter (inArea tiles) $ rectangles tiles

part2 :: Solver
part2 = show . last . sort . map (uncurry area) . rectanglesInArea . map tile . lines

tests :: Tests
tests =
  [ show . length . lines
  ]

