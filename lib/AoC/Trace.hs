{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC.Trace
  ( trace
  , idtrace
  , ftrace
  , rtrace
  , rftrace
  , showCGrid
  , showCGrids
  , showGrid
  , showGrids
  ) where

import Data.List (intercalate, transpose)
import Debug.Trace (trace)

showGrid :: Show a => [[a]] -> String
showGrid = (++) "\n" . flip (++) "\n" . intercalate "\n" . map (intercalate " " . map show)

showCGrid :: [[Char]] -> String
showCGrid = (++) "\n" . flip (++) "\n" . intercalate "\n"

showGrids :: Show a => [[[a]]] -> String
showGrids gs = showCGrid [intercalate "    " $ map show z | z <- transpose gs]

showCGrids :: [[[Char]]] -> String
showCGrids gs = showCGrid [intercalate "    " z | z <- transpose gs]

idtrace :: (Show a) => a -> a
idtrace x = trace (show x) x

ftrace :: (a -> String) -> a -> a
ftrace f x = trace (f x) x

rftrace :: (a -> String) -> (b -> String) -> (a -> b) -> a -> b
rftrace showa showb f n = ftrace (\r -> showa n ++ " => " ++ showb r) $ f n

rtrace :: (Show a, Show b) => (a -> b) -> a -> b
rtrace f n = rftrace show show f n

