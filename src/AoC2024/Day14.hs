{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC2024.Day14
  ( part1
  , part2
  , tests
  ) where

import Data.List (group, sort)
import Data.List.Split (splitOn)
import Data.Matrix (Matrix, fromLists, rref, transpose, toLists, zero)
import Data.Either (fromRight)

import AoC (Solver, Tests)
import AoC.Pair (Pair, pair, unpair, add, multiply, combine)
import qualified AoC.Pair as P (toList)
import AoC.Grid (Coord, drawCoords, drawSGrid, go, Direction(R))
import AoC.Trace

type Robot = Pair (Coord Int)

robot :: String -> Robot
robot = pair . map (pair . map read . splitOn "," . drop 2) . splitOn " "

moveRobot :: Pair Int -> Int -> Robot -> Robot
moveRobot m n (p, v) = flip (,) v $ combine (flip mod) m $ add p $ multiply n v

data Quadrant = Center
              | TopLeft
              | TopRight
              | BottomLeft
              | BottomRight
  deriving (Eq, Ord, Show)

quadrant :: Pair Int -> Robot -> Quadrant
quadrant m ((x, y), _) | x < cx && y < cy = TopLeft
                       | x > cx && y < cy = TopRight
                       | x < cx && y > cy = BottomLeft
                       | x > cx && y > cy = BottomRight
                       | otherwise = Center
  where (cx, cy) = unpair (flip div 2) m

showRobots :: [Robot] -> String
showRobots = drawSGrid "" . drawCoords "🤖" "  " . map fst

steps :: Int -> [Robot] -> [Robot]
steps n rs = map (moveRobot m n) rs
  where m = bounds rs

bounds :: [Robot] -> Pair Int
bounds rs = (mx, my)
  where mx = (+) 1 $ maximum $ map (fst . fst) rs
        my = (+) 1 $ maximum $ map (snd . fst) rs

quadrants :: Int -> [Robot] -> [Quadrant]
quadrants n rs = map (quadrant m) $ ftrace showRobots $ steps n rs
-- quadrants n rs = map (quadrant m) $ steps n rs
  where m = bounds rs

noCenter :: [Quadrant] -> [Quadrant]
noCenter = filter (/= Center)

part1 :: Solver
part1 = show . product . map length . group . sort . noCenter . quadrants 100 . map robot . lines

isCluster :: Int -> [Quadrant] -> Bool
isCluster lim = any ((> lim) . length) . ftrace (show . maximum . map length) . group . sort

isTree :: [Robot] -> Int -> Bool
isTree rs n = any treeLine rs'
  where rs' = map fst $ steps n rs
        row p = take 10 $ (p:row (go R p))
        treeLine = all (flip elem rs') . row

findTree :: (Int -> Bool) -> Int -> Int -> Int
findTree match step n | match n = n
                      | otherwise = findTree match step (idtrace $ n + step)

part2 :: Solver
part2 = show . result . start . map robot . lines
  where result (s, step, match, rs) | length rs < 500 = 0  -- ignore other test inputs
                                    | otherwise = ftrace (showRobots . flip steps rs) $ findTree match step s
        start rs = (result (0, 1, isCluster 200 . flip quadrants rs, rs), (fst $ bounds rs), isTree rs, rs)

tests :: Tests
tests =
  [ show . map robot . lines
  , show . sum . map length . ftrace showRobots . steps 100 . map robot . lines
  , show . sum . map length . ftrace showRobots . steps 5 . map robot . lines
  -- , show . sum . map length . ftrace showRobots . steps 25 . map robot . lines
  , show . sum . map length . ftrace showRobots . steps 66 . map robot . lines -- +41
  -- , show . sum . map length . ftrace showRobots . steps 128 . map robot . lines -- +62
  , show . sum . map length . ftrace showRobots . steps 167 . map robot . lines -- +39
  -- , show . sum . map length . ftrace showRobots . steps 231 . map robot . lines -- +64
  -- , show . sum . map length . ftrace showRobots . steps 268 . map robot . lines -- +37
  -- , show . sum . map length . ftrace showRobots . steps 334 . map robot . lines -- +66
  -- , show . sum . map length . ftrace showRobots . steps 369 . map robot . lines -- +35
  -- , show . sum . map length . ftrace showRobots . steps 437 . map robot . lines -- +68
  -- , show . sum . map length . ftrace showRobots . steps 470 . map robot . lines -- +33
  -- , show . sum . map length . ftrace showRobots . steps 540 . map robot . lines -- +70
  , show . sum . map length . ftrace showRobots . steps 571 . map robot . lines -- +31
  -- , show . sum . map length . ftrace showRobots . steps 643 . map robot . lines -- +72
  -- , show . sum . map length . ftrace showRobots . steps 672 . map robot . lines -- +29
  -- , show . sum . map length . ftrace showRobots . steps 746 . map robot . lines -- +74
  ]

