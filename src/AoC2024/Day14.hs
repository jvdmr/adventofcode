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
import AoC.Util (Pair, pair, unpair, add, multiply, combine)
import qualified AoC.Util as P (toList)
import AoC.Grid (drawCoords, drawSGrid)
import AoC.Trace

type Robot = Pair (Pair Int)

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

steps :: Pair Int -> Int -> [Robot] -> [Robot]
steps m n rs = ftrace (drawSGrid "" . drawCoords "🤖" "  " . map fst . filter ((/=) Center . quadrant m)) $ map (moveRobot m n) rs

quadrants :: Int -> [Robot] -> [Quadrant]
quadrants n rs = map (quadrant (mx, my)) $ steps (mx, my) n rs
  where mx = (+) 1 $ maximum $ map (fst . fst) rs
        my = (+) 1 $ maximum $ map (snd . fst) rs

noCenter :: [Quadrant] -> [Quadrant]
noCenter = filter (/= Center)

part1 :: Solver
part1 = show . product . map length . group . sort . noCenter . quadrants 100 . map robot . lines

tracetree :: [Int] -> [Robot] -> Int
tracetree ns rs = sum $ map length $ map (flip (steps (mx, my)) rs . idtrace) ns
  where mx = (+) 1 $ maximum $ map (fst . fst) rs
        my = (+) 1 $ maximum $ map (snd . fst) rs

part2 :: Solver
-- part2 = show . tracetree [0..100000] . map robot . lines
part2 = show . tracetree [2188] . map robot . lines

findMiddles :: [Robot] -> ([Int], [Robot])
findMiddles rs = (ns, rs)
  where mx = (+) 1 $ maximum $ map (fst . fst) rs
        my = (+) 1 $ maximum $ map (snd . fst) rs
        (cx, cy) = unpair (flip div 2) (mx, my)
        topOfTree = (cx, 0)
        toc (p, v) = last $ toLists $ transpose $ fromRight (zero 2 3) $ rref $ transpose $ fromLists $ map (map toRational . P.toList) [p, v, topOfTree]
        ns = [5]

tests :: Tests
tests =
  [ show . tracetree [0] . map robot . lines
  -- , show . tracetree [2188] . map robot . lines
  , show . tracetree [0..100000] . map robot . lines
--   , show . uncurry tracetree . findMiddles . map robot . lines
  ]

