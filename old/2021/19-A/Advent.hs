module Main where

-- import Data.List
import Data.List.Split

import Debug.Trace
idtrace x = trace (show x) x

uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq [a] = [a]
uniq (a:b:rst) | a == b = uniq (b:rst)
               | otherwise = a:uniq (b:rst)

type Coord = (Int, Int, Int)

add :: Coord -> Coord -> Coord
add (x, y, z) (x', y', z') = (x + x', y + y', z + z')

diff :: Coord -> Coord -> Coord
diff (x, y, z) (x', y', z') = (x - x', y - y', z - z')

type Orientation = (Coord, Coord)

data Scanner = Scanner Coord Orientation [Coord]
  deriving (Show)

instance Eq Scanner where
  (==) (Scanner a _ _) (Scanner b _ _) = a == b

instance Ord Scanner where
  compare (Scanner a _ _) (Scanner b _ _) = compare a b

pos :: Scanner -> Coord
pos (Scanner p _ _) = p

beacons :: Scanner -> [Coord]
beacons (Scanner _ _ b) = b

parseBeacon :: String -> Coord
parseBeacon s = (a, b, c)
  where [a, b, c] = take 3 $ (++ [0]) $ map read $ splitOn "," s

parseScanner :: [String] -> Scanner
parseScanner (_:beacons) = Scanner (0, 0, 0) ((1, 0, 0), (0, 1, 0)) $ map parseBeacon beacons

orientations = [(f, u) | f <- [(1, 0, 0), (0, 1, 0), (-1, 0, 0), (0, -1, 0), (0, 0, 1), (0, 0, -1)],
                         u <- [(0, 1, 0), (0, 0, 1), (0, -1, 0), (0, 0, -1)]]

faceC :: Coord -> Coord -> Coord
faceC (x', y', z') (x, y, z) = (x * x' + y * y' + z * z', x * y' * (-1) + y * (x' + abs z'), x * z' * (-1) + z * abs (x' + y'))

rotateC :: Coord -> Coord -> Coord
rotateC (_, y', z') (x, y, z) = (x, y * y' + z * z', y * z' * (-1) + z * y')

orientC :: Orientation -> Coord -> Coord
orientC (f, u) c = rotateC u $ faceC f c

locate :: Coord -> Scanner -> Scanner
locate p (Scanner _ o b) = Scanner p o $ map (add p) b

rotate :: Orientation -> Scanner -> Scanner
rotate o@(f, u) (Scanner p _ b) = Scanner p o $ map (orientC o) b

match12 :: Scanner -> Scanner -> Scanner
match12 (Scanner _ _ ba) b@(Scanner _ _ bb) = case filter ((== 12) . length) $ group $ sort $ concat $ map (\a -> map (diff a) bb) ba of
                                                   [] -> b
                                                   [(c:_)] -> locate c b

matchScanners' :: [Scanner] -> Scanner -> [Scanner]
matchScanners' other origin = origin:(matched ++ otherMatches)
  where checked = map (match12 origin) other
        matched = filter ((/= (0, 0, 0)) . pos) checked
        unmatched = filter ((== (0, 0, 0)) . pos) checked
        otherMatches = uniq $ sort $ concat $ map (matchScanners' unmatched) matched


matchScanners :: [Scanner] -> [Scanner]
matchScanners (origin:other) = matchScanners' (concat $ map (\o -> map (rotate o) other) orientations) origin

main = do
  cnt <- getContents
  print $ length $ uniq $ sort $ concat $ map beacons $ idtrace $ matchScanners $ map parseScanner $ splitOn [""] $ lines cnt

