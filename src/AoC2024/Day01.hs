{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC2024.Day01
  ( part1
  , part2
  ) where

import Data.List (transpose, sort)

import AoC (Solver)

uncurryL :: (a -> a -> b) -> [a] -> b
uncurryL f (a:b:_) = f a b

part1 :: Solver
part1 = show . sum . map abs . uncurryL (zipWith (-)) . map sort . transpose . map (map read . words) . lines

similarity :: [Int] -> [Int] -> [Int]
similarity [] _ = []
similarity (a:as) bs = ((a *) $ length $ filter (== a) bs):similarity as bs

part2 :: Solver
part2 = show . sum . uncurryL similarity . transpose . map (map read . words) . lines

