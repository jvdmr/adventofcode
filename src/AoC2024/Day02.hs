{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC2024.Day02
  ( part1
  , part2
  ) where

import AoC (Solver)
import AoC.Util (zipTailWith, between, count, skipOne)

check :: [Int] -> Bool
check diffs@(a:_) = btwn && sgn
  where btwn = all (between 1 3 . abs) diffs
        sgn = all (> 0) $ map (* a) diffs

safe :: [Int] -> Bool
safe = check . zipTailWith (-)

part1 :: Solver
part1 = show . count safe . map (map read . words) . lines

safe' :: [Int] -> Bool
safe' levels = any safe $ levels:skipOne levels

part2 :: Solver
part2 = show . count safe' . map (map read . words) . lines

