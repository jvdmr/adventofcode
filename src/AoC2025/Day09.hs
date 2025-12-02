{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC2025.Day09
  ( part1
  , part2
  , tests
  ) where

import Prelude hiding (foldl) -- use foldl' instead of foldl

import AoC (Solver, Tests)

part1 :: Solver
part1 = ("Not yet solved! " ++) . show . length . lines

part2 :: Solver
part2 = ("Not yet solved! " ++) . show . length . lines

tests :: Tests
tests =
  [ show . length . lines
  ]

