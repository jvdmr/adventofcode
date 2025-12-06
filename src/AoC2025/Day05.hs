{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC2025.Day05
  ( part1
  , part2
  , tests
  ) where

import Prelude hiding (foldl) -- use foldl' instead of foldl
import Data.List.Split (splitOn)

import AoC (Solver, Tests)
import AoC.Pair (Pair(..), pair)
import AoC.Util (between, ignoreInput)
-- import AoC.Trace (idtrace)

type Range = Pair Int

toRange :: String -> Range
toRange = pair . map read . splitOn "-"

inRange :: Range -> Int -> Bool
inRange = uncurry between

freshIngredients :: [String] -> [Int]
freshIngredients input = filter check $ map read ingredients
  where [freshRangesStrings, ingredients] = splitOn [""] input
        freshRanges = map toRange freshRangesStrings
        check n = any (flip inRange n) freshRanges

part1 :: Solver
part1 = show . length . freshIngredients . lines

mergeRange :: [Range] -> [Range] -> [Range]
mergeRange [] [] = []
mergeRange [r] [] = [r]
mergeRange (r1:r2:rs) [] = r1:mergeRange [r2] rs
mergeRange (r1@(a, b):rs1) (r2@(c, d):rs2) | inRange r1 c && inRange r1 d = mergeRange (r1:rs1) rs2
                                           | inRange r1 c = mergeRange [(a, d)] $ rs1 ++ rs2
                                           | inRange r1 d = mergeRange [(c, b)] $ rs1 ++ rs2
                                           | inRange r2 a = mergeRange [r2] $ rs1 ++ rs2
                                           | otherwise = mergeRange (r1:r2:rs1) rs2

mergeRanges :: [Range] -> [Range]
mergeRanges (r:rs) = mergeRange [r] rs

rangeLength :: Range -> Int
rangeLength (a, b) = b - (a - 1)

allFreshIngredients :: [String] -> Int
allFreshIngredients = sum . map rangeLength . mergeRanges . map toRange . head . splitOn [""]
-- allFreshIngredients = sum . map rangeLength . idtrace . mergeRanges . map toRange . head . splitOn [""]

part2 :: Solver
part2 = show . allFreshIngredients . lines

tests :: Tests
tests =
  [ show . length . lines
  , ignoreInput $ show $ allFreshIngredients ["1-4", "3-6"]
  , ignoreInput $ show $ allFreshIngredients ["1-6", "2-3"]
  , ignoreInput $ show $ allFreshIngredients ["2-6", "1-3"]
  , ignoreInput $ show $ allFreshIngredients ["2-3", "1-6"]
  , ignoreInput $ show $ allFreshIngredients ["1-3", "5-7"]
  , ignoreInput $ show $ allFreshIngredients ["1-4", "3-6", "2-5"]
  , ignoreInput $ show $ allFreshIngredients ["1-4", "2-3", "1-6"]
  , ignoreInput $ show $ allFreshIngredients ["2-6", "1-3", "4-5"]
  , ignoreInput $ show $ allFreshIngredients ["2-6", "1-3", "3-5"]
  , ignoreInput $ show $ allFreshIngredients ["2-3", "1-4", "5-6"]
  , ignoreInput $ show $ allFreshIngredients ["1-3", "5-6", "3-5"]
  ]

