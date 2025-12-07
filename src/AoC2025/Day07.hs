{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC2025.Day07
  ( part1
  , part2
  , tests
  ) where

import Prelude hiding (foldl) -- use foldl' instead of foldl

import AoC (Solver, Tests)

type ManifoldCheck = Char -> Bool

isBeam :: ManifoldCheck
isBeam 'S' = True
isBeam '|' = True
isBeam _ = False

isSplitter :: ManifoldCheck
isSplitter '^' = True
isSplitter _ = False

type ManifoldLocation = (Int, Int)  -- index, # of timelines

initTimeline :: (Int, a) -> ManifoldLocation
initTimeline (i, _) = (i, 1)

indexes :: ManifoldCheck -> [Char] -> [ManifoldLocation]
indexes cf = map initTimeline . filter (cf . snd) . zip [0..]

manifold :: [[Char]] -> [[ManifoldLocation]]
manifold (l1:ls) = (indexes isBeam l1):map (indexes isSplitter) ls

type ManifoldMatch = [ManifoldLocation] -> [ManifoldLocation] -> Int

-- faster than `length . filter elem` but only works for sorted lists
-- ignore timelines
matching1 :: ManifoldMatch
matching1 [] _ = 0
matching1 _ [] = 0
matching1 ((a, _):as) ((b, _):bs) | a == b = 1 + matching1 as bs
                                  | a > b = matching1 ((a, 1):as) bs
                                  | a < b = matching1 as ((b, 1):bs)

-- faster than `nub . concat` but only works for sorted lists
merge :: [[ManifoldLocation]] -> [ManifoldLocation]
merge [] = []
merge [[]] = []
merge [[a]] = [a]
merge ([]:rst) = merge rst
merge ([(a, at)]:((b, bt):bs):rst) | a == b = (a, at + bt):merge (bs:rst)
                                   | otherwise = (a, at):merge (((b, bt):bs):rst)
merge ((a:as):rst) = a:merge (as:rst)

left :: ManifoldLocation -> ManifoldLocation
left (a, at) = (a - 1, at)

right :: ManifoldLocation -> ManifoldLocation
right (a, at) = (a + 1, at)

splitBeams :: ManifoldMatch -> Int -> [[ManifoldLocation]] -> Int
splitBeams _ splits [_] = splits
splitBeams mf splits (cur:nxt:rst) = splitBeams mf splits' (nxt':rst)
  where splits' = splits + newSplits
        newSplits = mf cur nxt
        nxt' = merge $ map splitOrContinue cur
        splitOrContinue beam | 1 == matching1 [beam] nxt = [left beam, right beam]
                             | otherwise = [beam]

part1 :: Solver
part1 = show . splitBeams matching1 0 . manifold . lines

-- faster than `length . filter elem` but only works for sorted lists
-- count timelines
matching2 :: ManifoldMatch
matching2 [] _ = 0
matching2 _ [] = 0
matching2 ((a, at):as) ((b, bt):bs) | a == b = at + matching2 as bs
                                    | a > b = matching2 ((a, at):as) bs
                                    | a < b = matching2 as ((b, bt):bs)

part2 :: Solver
part2 = show . splitBeams matching2 1 . manifold . lines

tests :: Tests
tests =
  [ show . length . lines
  ]

