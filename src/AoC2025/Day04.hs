{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC2025.Day04
  ( part1
  , part2
  , tests
  ) where

import Prelude hiding (foldl) -- use foldl' instead of foldl

import AoC (Solver, Tests)
import AoC.Grid (Grid(..), Coord(..), coordsWhere', (!), surround, inGrid, insertAt)
-- import AoC.Grid (drawGrid)
-- import AoC.Trace (ftrace)

type PrintFloorCoord = Coord Int
type PrintFloorTile = Char
type PrintFloor = Grid PrintFloorTile

isPaper :: PrintFloorTile -> Bool
isPaper '@' = True
isPaper _ = False

isAccessiblePaper :: PrintFloor -> PrintFloorCoord -> Bool
isAccessiblePaper printfloor c = isPaper (printfloor ! c) && length surroundingPaper < 4
  where surroundingPaper = filter isPaper $ map ((!) printfloor) $ filter (inGrid printfloor) $ surround c

accessiblePaper :: PrintFloor -> [PrintFloorCoord]
accessiblePaper printfloor = coordsWhere' (isAccessiblePaper printfloor) printfloor
-- accessiblePaper printfloor = ftrace showPrintfloor $ coordsWhere' (isAccessiblePaper printfloor) printfloor
--   where showPrintfloor locs = drawGrid $ insertAt printfloor 'x' locs

part1 :: Solver
part1 = show . length . accessiblePaper . Grid . lines

removePaper' :: PrintFloor -> [PrintFloorCoord] -> [PrintFloorCoord] -> [PrintFloorCoord]
removePaper' _ result [] = result
removePaper' printfloor result toRemove = removePaper' printfloor' (toRemove ++ result) $ accessiblePaper printfloor'
  where printfloor' = insertAt printfloor 'x' toRemove

removePaper :: PrintFloor -> [PrintFloorCoord]
removePaper printfloor = removePaper' printfloor [] $ accessiblePaper printfloor

part2 :: Solver
part2 = show . length . removePaper . Grid . lines

tests :: Tests
tests =
  [ show . length . lines
  ]

