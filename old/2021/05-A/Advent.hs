module Main where

import Debug.Trace
-- import Data.List
import Data.List.Split

idtrace x = trace (show x) x

type Point = (Int, Int)
type Line = (Point, Point)
type ExpandedLine = [Point]

x :: Point -> Int
x (r, _) = r

y :: Point -> Int
y (_, r) = r

x1 :: Line -> Int
x1 (r, _) = x r

x2 :: Line -> Int
x2 (_, r) = x r

y1 :: Line -> Int
y1 (r, _) = y r

y2 :: Line -> Int
y2 (_, r) = y r

expandLine :: Line -> ExpandedLine
expandLine (p1@(x1, y1), p2@(x2, y2)) | x1 == x2 = map ((,) x1) [(min y1 y2)..(max y1 y2)]
                                      | y1 == y2 = map (\i -> (i, y1)) [(min x1 x2)..(max x1 x2)]
                                      | otherwise = [p1, p2]

cartesianFilter :: [Line] -> [Line]
cartesianFilter = filter (\l -> x1 l == x2 l || y1 l == y2 l)

parseLine :: String -> Line
parseLine sl = toLine $ map (map read . splitOn ",") $ splitOn " -> " sl
  where toLine [[a1, b1], [a2, b2]] = ((a1, b1), (a2, b2))

main = do
  cnt <- getContents
  print $ length $ idtrace $ map head $ filter ((> 1) . length) $ group $ sort $ concat $ map expandLine $ cartesianFilter $ map parseLine $ lines cnt

