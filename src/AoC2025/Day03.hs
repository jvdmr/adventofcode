{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC2025.Day03
  ( part1
  , part2
  , tests
  ) where

import Prelude hiding (foldl) -- use foldl' from Data.List if you need foldl

import AoC (Solver, Tests)

tests :: Tests
tests =
  [ show . length . lines
  ]

part1 :: Solver
part1 = ("Not yet solved! " ++) . show . length . lines

part2 :: Solver
part2 = ("Not yet solved! " ++) . show . length . lines

