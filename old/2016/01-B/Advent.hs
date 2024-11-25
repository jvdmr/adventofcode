module Main where

-- import Data.List
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

turn 'A' = id
turn 'R' = r
turn 'L' = l

type Direction = (Char, Int)

parseDirections :: String -> [Direction]
parseDirections = map direction . splitOn ", "
  where direction (o:n) = (o, read n)

type Coord = (Int, Int)

go :: Orientation -> Coord -> Coord
go N (x, y) = (x + 1, y)
go E (x, y) = (x, y + 1)
go S (x, y) = (x - 1, y)
go W (x, y) = (x, y - 1)

follow :: Orientation -> [Coord] -> [Direction] -> Coord
follow o coords ((_, 0):ins) = follow o coords ins
follow o (coord:coords) ((d, n):ins) | elem coord coords = coord
                                     | otherwise = follow no ((go no coord):coord:coords) (('A', n - 1):ins)
  where no = turn d o

taxi :: Coord -> Int
taxi (x, y) = abs x + abs y

main = do
  cnt <- getContents
  print $ map (taxi . idtrace . follow N [(0, 0)] . parseDirections) $ lines cnt

