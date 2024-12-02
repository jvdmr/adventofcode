{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC2024.Day02
  ( part1
  , part2
  ) where

import AoC (Solver)
import AoC.Util (between, count)

zipTailWith :: (a -> a -> b) -> [a] -> [b]
zipTailWith f a = zipWith f a $ tail a

check :: [Int] -> Bool
check diffs@(a:_) = btwn && sgn
  where btwn = all (between 1 3 . abs) diffs
        sgn = all (> 0) $ map (* a) diffs

safe :: [Int] -> Bool
safe = check . zipTailWith (-)

part1 :: Solver
part1 = show . count safe . map (map read . words) . lines

skipOne :: [a] -> [[a]]
skipOne [] = [[]]
skipOne (a:as) = as:map (a:) (skipOne as)

safe' :: [Int] -> Bool
safe' levels = any safe $ levels:skipOne levels

part2 :: Solver
part2 = show . count safe' . map (map read . words) . lines

