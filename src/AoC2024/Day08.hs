{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC2024.Day08
  ( part1
  , part2
  , test
  ) where

import Data.List (groupBy, sort, nub)

import AoC (Solver, Test)
import AoC.Util (equating)
import AoC.Grid

gatherGroup :: [(a, b)] -> (a, [b])
gatherGroup lst = (fst $ head lst, map snd lst)

antennas :: Grid Char -> [(Char, [Coord Int])]
antennas g = map gatherGroup $ groupBy (equating fst) $ sort [(g ! c, c) | c <- filter isAntenna $ coords g]
  where isAntenna loc = '.' /= g ! loc

antinode :: Grid a -> Coord Int -> Coord Int -> [Coord Int]
antinode g a b | add a d == b = filter (inGrid g) $ [add b d, add a (neg d)]
               | otherwise = filter (inGrid g) $ [add a d, add b (neg d)]
               where d = add a (neg b)

antinodes :: Grid a -> (Grid a -> Coord Int -> Coord Int -> [Coord Int]) -> (Char, [Coord Int]) -> [Coord Int]
antinodes _ _ (_, [_]) = []
antinodes g f (n, (c:cs)) = (concat $ map (f g c) cs) ++ antinodes g f (n, cs)

test :: Test
test = show . length . lines

part1 :: Solver
part1 = show . length . ans . Grid . lines
  where ans g = nub $ concat $ map (antinodes g antinode) $ antennas g

antinode' :: Grid a -> Coord Int -> Coord Int -> [Coord Int]
antinode' g a b = line ++ line'
  where (x, y) = add a (neg b)
        n = gcd x y
        d = (div x n, div y n)
        line = takeWhile (inGrid g) $ iterate (add d) a
        line' = takeWhile (inGrid g) $ tail $ iterate (add $ neg d) a

part2 :: Solver
part2 = show . length . ans . Grid . lines
  where ans g = nub $ concat $ map (antinodes g antinode') $ antennas g

