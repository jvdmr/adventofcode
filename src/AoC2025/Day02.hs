{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC2025.Day02
  ( part1
  , part2
  , tests
  ) where

import Prelude hiding (foldl) -- use foldl' instead of foldl
import Data.List.Split (chunksOf)

import AoC (Solver, Tests)
import AoC.Text (csl, range)

doubleRepeatingDigits :: Int -> Bool
doubleRepeatingDigits i = even l && take l' i' == drop l' i'
  where i' = show i
        l = length i'
        l' = div l 2

part1 :: Solver
part1 = show . sum . filter doubleRepeatingDigits . concat . map (range "-") . csl . head . lines

repeatingDigits :: Int -> Bool
repeatingDigits i = checkChunks 1
  where i' = show i
        l' = length i' `div` 2
        checkChunks n | n <= l' = let chunks = chunksOf n i' in all (== (head chunks)) chunks || checkChunks (n + 1)
                      | otherwise = False

part2 :: Solver
part2 = show . sum . filter repeatingDigits . concat . map (range "-") . csl . head . lines

tests :: Tests
tests =
  [ show . length . lines
  ]

