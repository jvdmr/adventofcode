module Main where

-- import Data.List
import Data.List.Split

import Debug.Trace
idtrace x = trace (show x) x
ftrace f x = trace (show $ f x) x
gridTrace rs cs = trace (concat $ map ((++"\n") . fill . map fst) $ groupBy (eq snd) $ sortBy compareYX (tot ++ filler)) cs
  where fill xs = map (s xs) [0..6]
        tot = rs ++ cs
        h = greatest $ map snd tot
        l = smallest $ map snd tot
        filler = map fakex [l..h]
        fakex y = (-1, y)
        s xs x | elem x xs = '#'
               | otherwise = ' '

eq :: Eq b => (a -> b) -> (a -> a -> Bool)
eq f a b = f a == f b

type Coord = (Int, Int)

compareYX (a, b) (c, d) | b == d = compare a c
                        | otherwise = compare d b

instance Num Coord where
  (+) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
  negate _ = undefined
  (*) _ _ = undefined
  fromInteger _ = undefined
  abs _ = undefined
  signum _ = undefined

type Grid a = [[a]]

val :: Grid a -> Coord -> a
val g (x, y) = (g !! y) !! x

data Rock = Rock Coord Int [Coord] -- location, right edge, pieces
  deriving (Eq, Show)

parseRock :: [[Char]] -> Rock
parseRock rock = Rock (0, 0) (greatest $ map fst pieces) pieces
  where pieces = concat $ zipWith (\y xs -> [(x, y) | x <- xs]) [0..] $ map (map fst . filter ((== '#') . snd) . zip [0..]) $ reverse rock

parseInput :: Int -> [[String]] -> ([Rock], [Char])
parseInput numberOfRocks input = (take numberOfRocks $ concat $ repeat $ map parseRock $ init input, concat $ repeat $ concat $ last input)

blast :: Char -> Rock -> Rock
blast '<' (Rock (0, y) r cs) = Rock (0, y) r cs
blast '<' (Rock (x, y) r cs) = Rock (x - 1, y) r cs
blast '>' (Rock (x, y) r cs) | r + x == 6 = Rock (x, y) r cs
                             | otherwise = Rock (x + 1, y) r cs

smallest :: (Show a, Bounded a, Ord a) => [a] -> a
smallest l = smallest' maxBound l
  where smallest' m [] = m
        smallest' m (a:as) = smallest' (min m a) as

greatest :: (Show a, Bounded a, Ord a) => [a] -> a
greatest l = greatest' minBound l
  where greatest' m [] = m
        greatest' m (a:as) = greatest' (max m a) as

inBounds :: Coord -> Bool
inBounds (x, _) = x >= 0 && x <= 6

neighbors known (x, y) = filter (flip notElem known) $ filter inBounds [(x - 1, y), (x, y + 1), (x + 1, y)]

findBound :: Grid Bool -> [Coord] -> [Coord] -> [Coord]
findBound _ _ [] = []
findBound g known (xy:rest) | val g xy = xy:findBound g (xy:known) rest
                            | otherwise = findBound g (xy:known) $ rest ++ neighbors (rest ++ known) xy

overrideY y (x, _) = (x, y)
overrideYs y cs = map (overrideY y) cs

reachable :: Int -> [Coord] -> [Coord]
reachable highest cs = concat $ zipWith overrideYs (reverse [0..highest]) $ reverse $ groupBy (eq snd) $ sortBy compareYX $ findBound tcs [] [(0, 0)]
  where tcs = map (fill . map fst) $ ([]:) $ groupBy (eq snd) cs
        fill xs = map (s xs) [0..6]
        s xs x = elem x xs

clean :: [Coord] -> [Coord]
clean cs = reachable h cs
  where h = greatest $ map snd cs

resolve :: Rock -> [Coord]
resolve (Rock xy _ cs) = map resolve' cs
  where resolve' a = xy + a

fits :: [Coord] -> Rock -> Bool
fits cs r = intersect cs (resolve r) == []

down :: Rock -> Rock
down (Rock (x, y) r cs) = Rock (x, y - 1) r cs

tetris :: [Coord] -> ([Rock], [Char]) -> Int
tetris filled ([], _) = greatest $ map snd filled
tetris filled ((rock:rocks), (b:blasts)) | fits filled drock = tetris filled (drock:rocks, blasts)
                                         | otherwise = tetris (clean (sortBy compareYX $ (resolve brock) ++ filled)) (rocks, blasts)
--                                          | otherwise = tetris (gridTrace [] $ clean (sortBy compareYX $ (resolve brock) ++ filled)) (rocks, blasts)
  where drock = down brock
        brock | fits filled rock' = rock'
              | otherwise = irock
        rock' = blast b irock
        irock = initialize rock
        initialize (Rock (0, 0) r cs) = Rock (2, highest + 4) r cs
        initialize r = r
        highest = greatest $ map snd filled

main = do
  cnt <- getContents
  print $ tetris [(x, 0) | x <- [0..6]] $ parseInput 2022 $ splitOn [""] $ lines cnt

