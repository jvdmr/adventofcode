module Main where

import Data.List
import Data.List.Split

import Debug.Trace
idtrace x = trace (show x) x

type Coord = (Int, Int)
type Sensor = Coord
type Beacon = Coord
type Range = (Int, Int)

pair :: [a] -> (a, a)
pair [a, b] = (a, b)

uniq [] = []
uniq [a] = [a]
uniq (a:b:rst) | a == b = uniq (b:rst)
               | otherwise = a:uniq (b:rst)

between a b c = a <= b && b <= c

parseSensor :: String -> (Sensor, Beacon)
parseSensor l = pair $ map (pair . map (read . head . tail . splitOn "=") . splitOn "," . concat . tail . dropWhile (/= "at") . words) $ splitOn ":" l

parseCoverageMap :: [String] -> (Int, [(Sensor, Beacon)])
parseCoverageMap (yline:sensors) = (read yline, map parseSensor sensors)

coverageRangeInLine :: Int -> (Sensor, Beacon) -> [(Int, Int)]
coverageRangeInLine y ((sx, sy), (bx, by)) | between (sy - distance) y (sy + distance) = [(a, b)]
                                           | otherwise = []
  where distance = abs (sx - bx) + abs (sy - by)
        a = sx - distance + abs (sy - y)
        b = sx + distance - abs (sy - y)

overlap :: Range -> Range -> Bool
overlap (a, b) (c, d) = between a c b || between a d b || between c a d || b + 1 == c || d + 1 == a

mergeRange :: Range -> Range -> Range
mergeRange (a, b) (c, d) | between a c b && between a d b = (a, b)
                         | between a c b && between c b d = (a, d)
                         | between c a d && between a d b = (c, b)
                         | between c a d && between c b d = (c, d)
                         | b + 1 == c = (a, d)
                         | d + 1 == a = (c, b)

mergeRanges :: [Range] -> Range -> [Range]
mergeRanges [] r = [r]
mergeRanges (a:rest) r | overlap a r = mergeRanges rest $ mergeRange a r
                       | otherwise = a:mergeRanges rest r

limitRange :: Range -> Range -> [Range]
limitRange (ma, mb) (a, b) | between ma a mb || between ma b mb || between a ma b = [(max ma a, min mb b)]
                           | otherwise = []

coverageForLine :: [(Sensor, Beacon)] -> Int -> [Range]
coverageForLine sensors y = foldl mergeRanges [] $ concat $ map (coverageRangeInLine y) sensors

findHole :: (Int, [Range]) -> Coord
findHole (y, [(_, b), (_, _)]) = (b + 1, y)

coverageForRange (max, sensors) = findHole $ head $ idtrace $ filter ((/= [(0, max)]) . snd) $ zip [0..] $ map (sort . concat . map (limitRange (0, max)) . coverageForLine sensors) [0..max]

tuning :: Coord -> Int
tuning (x, y) = x * 4000000 + y

main = do
  cnt <- getContents
  print $ tuning $ coverageForRange $ parseCoverageMap $ lines cnt

