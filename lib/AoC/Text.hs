{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC.Text
  ( csl  -- comma-separated list
  , range  -- expand <number><separator><number> to list of numbers
  ) where

import Data.List.Split (splitOn)

import AoC.Pair (pair)

csl :: String -> [String]
csl = splitOn ","

range :: String -> String -> [Int]
range separator = uncurry enumFromTo . pair . map read . splitOn separator
