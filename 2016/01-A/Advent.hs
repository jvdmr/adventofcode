module Main where

import Data.List
import Data.List.Split

import Debug.Trace
idtrace x = trace (show x) x

data Orientation = N | E | S | W
  deriving (Eq, Show)

r N = E
r E = S
r S = W
r W = N

l N = W
l E = N
l S = E
l W = S

turn 'R' = r
turn 'L' = l

parseDirections = splitOn ", "

type Coord = (Int, Int)

go :: Orientation -> Int -> Coord -> Coord
go N n (x, y) = (x + n, y)
go E n (x, y) = (x, y + n)
go S n (x, y) = (x - n, y)
go W n (x, y) = (x, y - n)

follow :: Orientation -> Coord -> [String] -> Coord
follow _ coord [] = coord
follow o coord ((d:n):ins) = follow no (go no (read n) coord) ins
  where no = turn d o

taxi :: Coord -> Int
taxi (x, y) = abs x + abs y

main = do
  cnt <- getContents
  print $ map (taxi . idtrace . follow N (0, 0) . parseDirections) $ lines cnt

