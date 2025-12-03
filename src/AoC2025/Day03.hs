{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC2025.Day03
  ( part1
  , part2
  , tests
  ) where

import Prelude hiding (foldl) -- use foldl' instead of foldl
import Data.List (sort)

import AoC (Solver, Tests)

batteries :: [Char] -> [Int]
batteries = reverse . map (read . (:[]))

joltage :: Int -> [Int] -> Int
joltage 0 _ = 0
joltage n bank = (max * 10^n') + joltage n' bank'
  where n' = n - 1
        max = last $ sort $ drop n' bank
        bank' = reverse $ tail $ dropWhile (/= max) $ reverse bank

part1 :: Solver
part1 = show . sum . map (joltage 2 . batteries) . lines

part2 :: Solver
part2 = show . sum . map (joltage 12 . batteries) . lines

tests :: Tests
tests =
  [ show . length . lines
  ]

