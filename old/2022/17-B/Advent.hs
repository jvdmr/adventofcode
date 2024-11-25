module Main where

-- import Data.List
import Data.List.Split
import Data.Map ((!), fromList)
import qualified Data.Map as M

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
  deriving (Eq, Show, Ord)

parseRock :: [[Char]] -> Rock
parseRock rock = Rock (0, 0) (greatest $ map fst pieces) pieces
  where pieces = concat $ zipWith (\y xs -> [(x, y) | x <- xs]) [0..] $ map (map fst . filter ((== '#') . snd) . zip [0..]) $ reverse rock

parseInput :: [[String]] -> ([Rock], [Char])
parseInput input = (concat $ repeat $ map parseRock $ init input, concat $ repeat $ concat $ last input)

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

reset :: [Coord] -> (Int, [Coord])
reset cs = (low, map (+ (0, -low)) cs)
  where low = smallest $ map snd cs

tetris'' :: Rock -> Int -> [Char] -> [Coord] -> (Int, [Coord])
tetris'' rock bn (b:blasts) m | fits m drock = tetris'' drock (bn + 1) blasts m
                              | otherwise = (bn + 1, clean $ sortBy compareYX $ m ++ resolve brock)
  where drock = down brock
        brock | fits m rock' = rock'
              | otherwise = rock
        rock' = blast b rock

type State = (Int, Int, [Coord], Int) -- number of rocks, number of blasts, map, base height
start = (0, 0, [(x, 0) | x <- [0..6]], 0)

tetris' :: [Rock] -> [Char] -> (State -> State)
tetris' rocks blasts (r, b, m, h) = (r + 1, b'', m'', h')
  where rock = head $ drop r rocks
        blasts' = drop b blasts
        h' = h + dropped
        (dropped, m'') = reset m'
        (b', m') = tetris'' (initialize rock) 0 blasts' m
        b'' = b + b'
        highest = greatest $ map snd m
        initialize (Rock (0, 0) right cs) = Rock (2, highest + 4) right cs

matching :: State -> State -> Bool
matching (_, _, a, _) (_, _, b, _) = a == b

tetris :: ([Rock], [Char]) -> Int -> State
tetris (rocks, blasts) n = (c * r + er, c * b + eb, gridTrace [] em, c * h + eh)
  where f = tetris' rocks blasts
        tortoise = iterate f start
        hare = iterate (f . f) start
        steps = zip tortoise hare
        (first@(fr, fb, fm, fh), cycle@(cr, cb, cm, ch)) = head $ dropWhile (not . uncurry matching) $ tail steps
        r = cr - fr
        b = cb - fb
        h = ch - fh
        c = div (n - fr) r
        rest = mod (n - fr) r
        state = (fr + c * r, fb + c * b, fm, fh + c * h)
        (er, eb, em, eh) = tortoise !! (fr + rest)

totalHeight :: State -> Int
totalHeight (_, _, m, h) = h + highest
  where highest = greatest $ map snd m

main = do
  cnt <- getContents
  let t = tetris $ parseInput $ splitOn [""] $ lines cnt
--   print $ totalHeight $ t 7
--   print $ totalHeight $ idtrace $ t 2022
  print $ totalHeight $ idtrace $ t 1000000000000

