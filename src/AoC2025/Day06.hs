{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC2025.Day06
  ( part1
  , part2
  , tests
  ) where

import Prelude hiding (foldl) -- use foldl' instead of foldl
import Data.List (transpose)
import Data.List.Split (splitOn)

import AoC (Solver, Tests)
import AoC.Util (multiply)

calculate :: [String] -> Int
calculate ("+":terms) = sum $ map read terms
calculate ("*":factors) = multiply $ map read factors
calculate problem = calculate $ reverse problem

part1 :: Solver
part1 = show . sum . map calculate . transpose . map words . lines

cephalopodFormatting :: [String] -> [[String]]
cephalopodFormatting problems = zipWith (:) (words $ last problems) $ map concat $ splitOn [[]] $ map words $ transpose $ init problems

part2 :: Solver
part2 = show . sum . map calculate . cephalopodFormatting . lines

tests :: Tests
tests =
  [ show . length . lines
  ]

