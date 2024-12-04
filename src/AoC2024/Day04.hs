{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC2024.Day04
  ( part1
  , part2
  , test
  ) where

import Data.List (transpose)
import Data.List.Split (splitOn)

import AoC (Solver, Test)
import AoC.Trace

diag :: a -> [[a]] -> [[a]]
diag fill = transpose . ds . transpose
  where ds [] = []
        ds (c:cs) = c:ds (map (fill:) cs)

part1 :: Solver
part1 = show . sum . map countW . transform . lines
  where countW = sum . map (flip (-) 1 . length . splitOn "XMAS")
        transform g = map ($ g) [id, map reverse, transpose, transpose . reverse, df, map reverse . df, df . reverse, map reverse . df . reverse]
        df = diag ' '

type Chunk = [[Char]]

chunks :: Int -> Int -> [[Char]] -> [Chunk]
chunks x y cs | length cs < y = []
              | length (head cs) < x = []
              | otherwise = (map (take x) $ take y cs):(chunks x y $ map tail $ take y cs) ++ chunks x y (tail cs)

xmas :: Chunk -> Bool
xmas c = any (== "MMASS") $ map (cutx . ($ c)) [df, map reverse . df, df . map reverse, map reverse . df . reverse]
  where cutx [a, _, b, _, c] = concat [a, b, c]
        df = map (filter (/= ' ')) . diag ' '

test :: Test
test = out . filter xmas . chunks 3 3 . lines
  where out cs = (showCGrids $ take 10 cs) ++ "Total chunks: " ++ show (length cs)

part2 :: Solver
part2 = show . length . filter xmas . chunks 3 3 . lines

