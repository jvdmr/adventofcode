{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC
  ( Solver (..)
  , Day (..)
  , Year (..)
  , Test (..)
  , YearTests (..)
  ) where

import Data.Map (Map)

type Solver = (String -> String)
type Day = (Solver, Solver)
type Year = Map Int Day
type Test = Solver
type YearTests = Map Int Test

