{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC.Trace
  ( idtrace
  , ftrace
  , rtrace
  , showGrid
  , showGrids
  ) where

import Data.List (intercalate, transpose)
import Debug.Trace (trace)

showGrid :: [[Char]] -> String
showGrid g = intercalate "\n" g

showGrids :: [[[Char]]] -> String
showGrids gs = intercalate "\n" [intercalate "  " z | z <- transpose gs]

idtrace :: (Show a) => a -> a
idtrace x = trace (show x) x

ftrace :: (a -> String) -> a -> a
ftrace f x = trace (f x) x

rtrace :: (Show a, Show b) => (a -> b) -> a -> b
rtrace f n = ftrace (\r -> show n ++ " => " ++ show r) $ f n

