{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC
  ( Solver (..)
  , Day (..)
  , Year (..)
  , Tests (..)
  , YearTests (..)
  ) where

import Data.Map (Map)

type Solver = (String -> String)
type Day = (Solver, Solver)
type Year = Map Int Day
type Tests = [Solver]
type YearTests = Map Int Tests

