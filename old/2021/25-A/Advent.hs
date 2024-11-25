module Main where

-- import Data.List
import Data.List.Split

import Debug.Trace
ftrace :: (a -> String) -> a -> a
ftrace f x = trace (f x) x

idtrace :: (Show a) => a -> a
idtrace = ftrace show

type Coord = (Int, Int)

type Grid = [[Char]]

showGrid :: Grid -> String
showGrid = concat . map (++"\n")

coords :: Grid -> [Coord]
coords grid = [(x, y) | x <- [0..mx], y <- [0..my]]
  where mx = (length grid) - 1
        my = (length $ head grid) - 1

(!) :: Grid -> Coord -> Char
(!) grid (x, y) = grid !! x !! y

down :: Grid -> Coord -> Coord
down grid (x, y) = (mod (x + 1) mx, y)
  where mx = length grid

up :: Grid -> Coord -> Coord
up grid (x, y) = (mod (x - 1) mx, y)
  where mx = length grid

right :: Grid -> Coord -> Coord
right grid (x, y) = (x, mod (y + 1) my)
  where my = length $ head grid

left :: Grid -> Coord -> Coord
left grid (x, y) = (x, mod (y - 1) my)
  where my = length $ head grid

moveRight :: Grid -> Coord -> Char
moveRight grid c = case grid ! c of
                        'v' -> 'v'
                        '>' -> case grid ! right grid c of
                                    '.' -> '.'
                                    _ -> '>'
                        '.' -> case grid ! left grid c of
                                    '>' -> '>'
                                    _ -> '.'

moveDown :: Grid -> Coord -> Char
moveDown grid c = case grid ! c of
                       '>' -> '>'
                       'v' -> case grid ! down grid c of
                                   '.' -> '.'
                                   _ -> 'v'
                       '.' -> case grid ! up grid c of
                                   'v' -> 'v'
                                   _ -> '.'

moveF :: (Grid -> Coord -> Char) -> Grid -> Grid
moveF f grid = chunksOf my $ map (f grid) $ coords grid
  where my = length $ head grid

move :: (Grid, Grid) -> (Grid, Grid)
move (_, grid) = (grid, ftrace showGrid $ moveF moveDown $ moveF moveRight grid) -- leaving the debug output 'cause it looks cool

main = do
  cnt <- getContents
  print $ length $ takeWhile (uncurry (/=)) $ iterate move ([], lines cnt)

