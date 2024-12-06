{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC2024.Day06
  ( part1
  , part2
  , test
  ) where

import AoC (Solver, Test)
import AoC.Util (andF, count)
import AoC.Grid

type Guard = (Direction, Coord Int)
data Floor = Empty | Obstacle | Visited Direction
  deriving (Eq)
type Lab = Grid Floor

instance Show Floor where
  show Empty = " "
  show Obstacle = "#"

parseFloor :: Char -> Floor
parseFloor '#' = Obstacle
parseFloor _ = Empty

parseInput :: [String] -> (Lab, Guard)
parseInput s = (lab, (gd, gc))
  where sl = Grid s
        lab = mapG parseFloor sl
        gc = head $ filter (flip notElem ".#" . (!) sl) $ coords sl
        gd = parseDirection $ sl ! gc

moveWhile :: (Lab -> Guard -> Bool) -> Lab -> Guard -> Lab
moveWhile check lab guard@(gd, gl) | check lab guard = moveWhile check lab' guard'
                                   | otherwise = lab
                                   where guard' = (clockwise gd, end $ last path)
                                         lab' = insertAt lab (Visited gd) path
                                         path = takeWhile (flip andF [inGrid lab, (/= Obstacle) . (!) lab]) $ iterate (go gd) gl
                                         end c | inGrid lab $ go gd c = c
                                               | otherwise = go gd c

inLab :: Lab -> Guard -> Bool
inLab lab (_, gl) = inGrid lab gl

isVisited :: Floor -> Bool
isVisited (Visited _) = True
isVisited _ = False

visited :: Lab -> Int
visited (Grid g) = count isVisited $ concat g

test :: Test
test = show . parseInput . lines

part1 :: Solver
part1 = show . visited . uncurry (moveWhile inLab) . parseInput . lines

part2 :: Solver
part2 = ("Not yet solved! " ++) . show . length . lines

