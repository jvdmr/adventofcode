{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC
  ( Solver (..)
  , Day (..)
  , Year (..)
  ) where

import Data.Map (Map)

type Solver = (String -> String)
type Day = (Solver, Solver)
type Year = Map Int Day

