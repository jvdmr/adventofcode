module Main where

import Data.List
import Data.List.Split

import Debug.Trace
idtrace x = trace (show x) x

ftrace :: (a -> String) -> a -> a
ftrace f x = trace (f x) x

type Coord = (Int, Int)

data CaveItem = Sand Coord
              | Rock Coord
              | Abyss
              deriving (Show)

instance Eq CaveItem where
  (Sand a) == (Sand b) = a == b
  (Rock a) == (Sand b) = a == b
  (Sand a) == (Rock b) = a == b
  (Rock a) == (Rock b) = a == b
  Abyss == Abyss = True
  _ == _ = False

compareYX (x1, y1) (x2, y2) = compare (y1, x1) (y2, x2)

instance Ord CaveItem where
  compare (Sand a) (Sand b) = compareYX a b
  compare (Rock a) (Sand b) = compareYX a b
  compare (Sand a) (Rock b) = compareYX a b
  compare (Rock a) (Rock b) = compareYX a b
  compare Abyss Abyss = EQ
  compare Abyss _ = LT
  compare _ Abyss = GT

loc (Sand l) = l
loc (Rock l) = l
loc Abyss = (-1, -1)

type Cave = [CaveItem]

expand :: Coord -> Coord -> [Coord]
expand (a, b) (c, d) | a == c = [(a, y) | y <- [min b d..max b d]]
                     | b == d = [(x, b) | x <- [min a c..max a c]]

expandPath :: [Coord] -> [Coord]
expandPath [end] = [end]
expandPath (start:stop:rest) = expand start stop ++ expandPath (stop:rest)

parsePath :: String -> [Coord]
parsePath l = map (pair . map read . splitOn ",") $ splitOn " -> " l
  where pair [a, b] = (a, b)

sortedNotElem :: Coord -> [Coord] -> Bool
sortedNotElem _ [] = True
sortedNotElem a (b:rest) | a == b = False
                         | Sand a < Sand b = True
                         | otherwise = sortedNotElem a rest

fallStep :: Int -> [Coord] -> Coord -> Coord
fallStep maxy cave l@(x, y) | y > maxy = l
                            | sortedNotElem (x, y + 1) cave = fallStep maxy cave (x, y + 1)
                            | sortedNotElem (x - 1, y + 1) cave = fallStep maxy cave (x - 1, y + 1)
                            | sortedNotElem (x + 1, y + 1) cave = fallStep maxy cave (x + 1, y + 1)
                            | otherwise = l

findDup :: Int -> [Coord] -> CaveItem
findDup maxy (a@(_, y):b:rest) | y > maxy = Sand a
                               | a == b = Sand a
                               | otherwise = findDup maxy (b:rest)

fullCave :: CaveItem -> Bool
fullCave (Sand (500, 0)) = True
fullCave _ = False

showCaveItem (Sand _) = 'o'
showCaveItem (Rock _) = '#'
showCaveItem Abyss = ' '

traceCave :: Coord -> Cave -> Cave
traceCave from cave = trace (show $ length $ concat $ map idtrace $ chunksOf (maxx - minx + 1) $ map showLoc locs) cave
  where maxy = last $ sort $ map (snd . loc) cave
        xs = sort $ map (fst . loc) cave
        maxx = last xs
        minx = head xs
        locs = sortBy compareYX [(x, y) | x <- [minx..maxx], y <- [0..maxy]]
        fromMaybe Nothing = Abyss
        fromMaybe (Just a) = a
        showLoc l | l == from = '+'
                  | otherwise = showCaveItem $ fromMaybe $ find (== Sand l) cave

fall :: Coord -> Cave -> Cave
fall from cave | fullCave result = traceCave from cave
               | otherwise = fall from $ sort (result:cave)
  where result = findDup maxy $ iterate (fallStep maxy (map loc $ sort cave)) from
        maxy = last $ sort $ map (snd . loc) cave

isSand (Sand _) = True
isSand _ = False

main = do
  cnt <- getContents
  print $ length $ filter isSand $ fall (500, 0) $ map Rock $ concat $ map (expandPath . parsePath) $ lines cnt

