{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC2024.Day10
  ( part1
  , part2
  , tests
  ) where

import Data.List (nub)

import AoC (Solver, Tests)
import AoC.Util (readChar)
import AoC.Grid
import AoC.Bfs (bfs)

type HikeMap = Grid Int
type Step = Coord Int

isTrailHead :: HikeMap -> Step -> Bool
isTrailHead g c = 0 == g ! c

trailHeads :: HikeMap -> [Step]
trailHeads g = filter (isTrailHead g) $ coords g

nextSteps :: HikeMap -> [Step] -> [[Step]]
nextSteps g cs = map (:cs) $ filter ((== nh) . (!) g) $ filter (inGrid g) $ map (flip go c) [U, R, D, L]
  where nh = 1 + g ! c
        c = head cs

trails :: HikeMap -> Step -> [[Step]]
trails g c = map fst $ filter ((== 9) . snd) $ bfs (nextSteps g) [c]

rating :: HikeMap -> Step -> Int
rating g c = length $ trails g c

score :: HikeMap -> Step -> Int
score g c = length $ nub $ map head $ trails g c

tests :: Tests
tests = [show . length . lines]

part1 :: Solver
part1 = show . sum . scores . mapG readChar . Grid . lines
  where scores g = map (score g) $ trailHeads g

part2 :: Solver
part2 = show . sum . ratings . mapG readChar . Grid . lines
  where ratings g = map (rating g) $ trailHeads g

