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

nextlvl :: Cave -> [Cave]
nextlvl [] = []
nextlvl ((Sand (x, y)):rest) = [Sand (x', y + 1) | x' <- [x-1..x+1]]:nextlvl rest

avoidClipping :: Cave -> Cave
avoidClipping [] = []
avoidClipping [a] = [a]
avoidClipping ((Sand a):(Sand b):rest) | a == b = avoidClipping ((Sand b):rest)
                                       | otherwise = (Sand a):avoidClipping ((Sand b):rest)
avoidClipping ((Rock a):(Rock b):rest) | a == b = avoidClipping ((Rock b):rest)
                                       | otherwise = (Rock a):avoidClipping ((Rock b):rest)
avoidClipping ((Rock a):(Sand b):rest) | a == b = avoidClipping ((Rock a):rest)
                                       | otherwise = (Rock a):avoidClipping ((Sand b):rest)
avoidClipping ((Sand a):(Rock b):rest) | a == b = avoidClipping ((Rock b):rest)
                                       | otherwise = (Sand a):avoidClipping ((Rock b):rest)

fill :: Int -> [Cave] -> [Cave]
fill maxy [cur] = [cur]
fill maxy (cur:nxt:rest) | (snd $ loc $ head cur) > maxy = [cur]
                         | otherwise = cur:fill maxy (filledNxt:rest)
                         where filledNxt = avoidClipping $ sort $ concat $ nxt:(nextlvl $ filter isSand cur)

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

addGroups :: [Cave] -> [Cave]
addGroups [a] = [a, []]
addGroups (a:b:rest) | a' + 1 == b' = a:addGroups (b:rest)
                     | otherwise = [a] ++ take (b' - a' - 1) (repeat []) ++ addGroups (b:rest)
                     where a' = snd $ loc $ head a
                           b' = snd $ loc $ head b

fall :: Coord -> Cave -> Cave
fall from cave = traceCave from $ sort $ concat $ fill maxy $ addGroups $ groupBy groupF $ sort $ (Sand from):cave
  where maxy = last $ sort $ map (snd . loc) cave
        groupF a b = (snd $ loc a) == (snd $ loc b)

isSand (Sand _) = True
isSand _ = False

main = do
  cnt <- getContents
  print $ length $ filter isSand $ fall (500, 0) $ map Rock $ concat $ map (expandPath . parsePath) $ lines cnt

