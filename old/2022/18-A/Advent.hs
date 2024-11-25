module Main where

-- import Data.List
import Data.List.Split

import Debug.Trace
idtrace x = trace (show x) x

type Coord = (Int, Int, Int)

cx (x, _, _) = x
cy (_, y, _) = y
cz (_, _, z) = z

coord :: [Int] -> Coord
coord [x, y, z] = (x, y, z)

instance Num Coord where
  (a, b, c) + (d, e, f) = (a + d, b + e, c + f)
  negate (a, b, c) = (negate a, negate b, negate c)
  _ * _ = undefined
  fromInteger _ = undefined
  abs _ = undefined
  signum _ = undefined

type Droplet = [Coord]

droplet :: [String] -> Droplet
droplet = map (coord . map read . splitOn ",")

directions = [(1, 0, 0), (0, 1, 0), (0, 0, 1)]

neighbors :: Coord -> [Coord]
neighbors xyz = map (xyz +) $ directions ++ map negate directions

surfaces :: Droplet -> Int
surfaces d = length $ filter (flip notElem d) $ concat $ map neighbors d

main = do
  cnt <- getContents
  print $ surfaces $ droplet $ lines cnt

