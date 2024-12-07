{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC2024.Day07
  ( part1
  , part2
  , test
  ) where

import Data.List.Split (splitOn)

import AoC (Solver, Test)
import AoC.Util (orF)

parseInput :: String -> [Int]
parseInput = map read . splitOn " " . filter (/= ':')

type Operator = (Int -> Int -> Int)

match :: [Operator] -> Int -> [Int] -> Bool
match _ result [value] = result == value
match ops result (a:b:vs) = orF (a, b) $ map ((.) (match ops result)) $ map ((.) (:vs)) $ map (uncurry) ops

possiblyTrue :: [Operator] -> [Int] -> Bool
possiblyTrue ops (result:values) = match ops result values

test :: Test
test = show . length . lines

part1 :: Solver
part1 = show . sum . map head . filter (possiblyTrue [(+), (*)]) . map parseInput . lines

(|||) :: Operator
(|||) a b = read $ show a ++ show b

part2 :: Solver
part2 = show . sum . map head . filter (possiblyTrue [(+), (*), (|||)]) . map parseInput . lines

