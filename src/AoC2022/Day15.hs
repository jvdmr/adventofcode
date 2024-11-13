{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC2022.Day15
  ( part1
  , part2
  ) where

import Data.List

import Vdmr.Generic

part1 :: Solver
part1 = ("Not yet solved! " ++) . show . head . lines

part2 :: Solver
part2 = ("Not yet solved! " ++) . show . (*2) . read . head . lines

