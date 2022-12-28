module Main where

import Data.Ord
import Data.List

import Debug.Trace
idtrace x = trace (show x) x
ftrace f x = trace (f x) x

eq :: Eq b => (a -> b) -> a -> a -> Bool
eq f a b = f a == f b

smallest :: (Show a, Bounded a, Ord a) => [a] -> a
smallest l = smallest' maxBound l
  where smallest' m [] = m
        smallest' m (a:as) = smallest' (min m a) as

greatest :: (Show a, Bounded a, Ord a) => [a] -> a
greatest l = greatest' minBound l
  where greatest' m [] = m
        greatest' m (a:as) = greatest' (max m a) as

type Coord = (Int, Int)

instance Num Coord where
  (+) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
  negate (a, b) = (-a, -b)
  (*) (x1, y1) (x2, y2) = (x1 * x2, y1 * y2)
  fromInteger n = (fromInteger n, fromInteger n)
  abs (a, b) = (abs a, abs b)
  signum _ = undefined

compareYX (x1, y1) (x2, y2) = compare (y1, x1) (y2, x2)

type Elf = Coord
type Elves = [Elf]

allCoords :: Coord -> [Coord]
allCoords (lx, ly) = [(x, y) | y <- [0..ly - 1], x <- [0..lx - 1]]

loc :: [[a]] -> Coord -> a
loc m (x, y) = m !! y !! x

findElves :: [[Char]] -> Elves
findElves input = filter ((== '#') . loc input) $ allCoords (lx, ly)
  where lx = length $ head input
        ly = length input

diffs :: [Coord]
diffs = [(x, y) | x <- [-1..1], y <- [-1..1], x /= 0 || y /= 0]

neighbors :: Coord -> [Coord]
neighbors xy = map (+ xy) diffs

norths :: [Coord] -> Coord -> [Coord]
norths ns (_, y) = filter ((== (y - 1)) . snd) ns
souths :: [Coord] -> Coord -> [Coord]
souths ns (_, y) = filter ((== (y + 1)) . snd) ns
easts :: [Coord] -> Coord -> [Coord]
easts ns (x, _) = filter ((== (x + 1)) . fst) ns
wests :: [Coord] -> Coord -> [Coord]
wests ns (x, _) = filter ((== (x - 1)) . fst) ns

north :: Coord -> Coord
north (x, y) = (x, y - 1)
south :: Coord -> Coord
south (x, y) = (x, y + 1)
east :: Coord -> Coord
east (x, y) = (x + 1, y)
west :: Coord -> Coord
west (x, y) = (x - 1, y)

recenter :: Elves -> Elves
recenter elves = map (subtract (minx, miny)) elves
  where minx = smallest $ map fst elves
        miny = smallest $ map snd elves

bounds :: Elves -> Coord
bounds elves = (1, 1) + (maxx, maxy)
  where maxx = greatest $ map fst elves
        maxy = greatest $ map snd elves

drawElves :: Elves -> String
drawElves elves = "\n=====================\n" ++ (concat $ map ((++ "\n") . map showElf) $ groupBy (eq snd) $ sortBy compareYX $ allCoords $ bounds elves')
  where elves' = recenter elves
        showElf xy | elem xy elves' = '#'
                   | otherwise = '.'

type MoveProposer = [Coord] -> Elf -> (Bool, (Coord, Elf))

mpdone :: MoveProposer
mpdone an e = (length an == 8, (e, e))

mpgo :: ([Coord] -> Coord -> [Coord]) -> (Coord -> Coord) -> MoveProposer
mpgo cf mf an e = (length (cf an e) == 3, (mf e, e))

mpstay :: MoveProposer
mpstay _ e = (True, (e, e))

moveProposers :: [MoveProposer]
moveProposers = [
      mpgo norths north,
      mpgo souths south,
      mpgo wests west,
      mpgo easts east] ++ moveProposers

proposeMove :: [MoveProposer] -> Elves -> Elf -> (Coord, Elf)
proposeMove mpfs elves elf = snd $ head $ dropWhile (not . fst) $ map (\f -> f availableNeighbors elf) mpfs'
  where availableNeighbors = filter (flip notElem elves) $ neighbors elf
        mpfs' = [mpdone] ++ take 5 mpfs ++ [mpstay]

propose :: [MoveProposer] -> Elves -> [(Coord, Elf)]
propose mpfs elves = map (proposeMove mpfs elves) elves

moveElf :: [(Coord, Elf)] -> Elves
moveElf [(dest, _)] = [dest]
moveElf multi = map snd multi

move :: [(Coord, Elf)] -> Elves
move proposals = concat $ map moveElf $ groupBy (eq fst) $ sortBy (comparing fst) proposals

step :: ([MoveProposer], Elves) -> ([MoveProposer], Elves)
-- step (mpfs, elves) = (tail mpfs, move $ propose mpfs elves)
step (mpfs, elves) = (tail mpfs, ftrace drawElves $ move $ propose mpfs elves)

addMPs :: Elves -> ([MoveProposer], Elves)
addMPs elves = (moveProposers, elves)

iterateUntilNoop :: (a -> a) -> (a -> a -> Bool) -> a -> [a]
iterateUntilNoop f ef x = map fst $ takeWhile (not . done) steps'
  where steps' = zip steps $ tail steps
        steps = iterate f x
        done (a, b) = ef a b

main = do
  cnt <- getContents
  print $ (+ 1) $ length $ iterateUntilNoop step (eq snd) $ addMPs $ findElves $ lines cnt

