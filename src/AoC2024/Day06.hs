{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC2024.Day06
  ( part1
  , part2
  , test
  ) where

import AoC (Solver, Test)
import AoC.Util (andF, count)
import AoC.Grid
-- import AoC.Trace

type Guard = (Direction, Coord Int)
data Floor = Empty | Obstacle | Visited Direction | NewObstacle
  deriving (Eq)
type Lab = Grid Floor

instance Show Floor where
  show Empty = " "
  show Obstacle = "#"
  show NewObstacle = "O"
  show (Visited d) = show d

parseFloor :: Char -> Floor
parseFloor '#' = Obstacle
parseFloor _ = Empty

parseInput :: [String] -> (Lab, Guard)
parseInput s = (lab, (gd, gc))
  where sl = Grid s
        lab = mapG parseFloor sl
        gc = head $ filter (flip notElem ".#" . (!) sl) $ coords sl
        gd = parseDirection $ sl ! gc

moveWhile :: (Lab -> Guard -> Bool) -> Lab -> Guard -> (Lab, Guard)
moveWhile check lab guard@(gd, gl) | check lab guard = moveWhile check lab' guard'
                                   | otherwise = (lab, guard)
                                   where guard' = (clockwise gd, end $ last path)
                                         -- lab' = idtrace $ insertAt lab (Visited gd) $ init' path
                                         lab' = insertAt lab (Visited gd) $ init' path
                                         path = takeWhile (flip andF [inGrid lab, (/= Obstacle) . (!) lab]) $ iterate (go gd) gl
                                         init' p | inGrid lab $ go gd $ last p = init p
                                                 | otherwise = p
                                         end c | inGrid lab $ go gd c = c
                                               | otherwise = go gd c

inLab :: Lab -> Guard -> Bool
inLab lab (_, gl) = inGrid lab gl

isVisited :: Floor -> Bool
isVisited (Visited _) = True
isVisited _ = False

visited :: (Lab, Guard) -> Int
visited (Grid g, _) = count isVisited $ concat g

test :: Test
test = show . parseInput . lines

part1 :: Solver
part1 = show . visited . move . parseInput . lines
  where move (lab, guard) = moveWhile inLab lab guard

notLooping :: Lab -> Guard -> Bool
notLooping lab (gd, gl) = inGrid lab gl && Visited gd /= lab ! gl

moveWhile' :: (Lab -> Guard -> Bool) -> Lab -> Guard -> [Coord Int] -> [Coord Int]
moveWhile' check lab guard@(gd, gl) newObstacles | check lab guard = moveWhile' check lab' guard' n'
                                                 | otherwise = newObstacles
  where guard' = (clockwise gd, end $ last path)
        lab' = insertAt lab (Visited gd) path
        path = takeWhile (flip andF [inGrid lab, (/= Obstacle) . (!) lab]) $ iterate (go gd) gl
        n' = newObstacles ++ lps
        lps = filter isLoop $ filter (not . isVisited . (!) lab) $ tail path
        isLoop c = let mlab = insert lab Obstacle c in inLab mlab $ snd $ moveWhile notLooping mlab guard
        end c | inGrid lab $ go gd c = c
              | otherwise = go gd c

part2 :: Solver
part2 = show . length . move . parseInput . lines
  where move (lab, guard) = moveWhile' inLab lab guard []

