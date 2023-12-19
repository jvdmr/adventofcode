{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module Vdmr.Generic
  ( Solver (..)
  , Day (..)
  , Year (..)
  , idtrace
  , ftrace
  , flatten
  , between
  , unjust
  , bfs
  , hexToDec 
  ) where

import Data.Char (digitToInt)
import Data.Map (Map)

import Debug.Trace (trace)

type Solver = (String -> String)
type Day = (Solver, Solver)
type Year = Map Int Day

idtrace :: (Show a) => a -> a
idtrace x = trace (show x) x

ftrace :: (a -> String) -> a -> a
ftrace f x = trace (f x) x

flatten :: [[a]] -> [a]
flatten = foldl (++) []

between :: Int -> Int -> Int -> Bool
between a b c = a <= b && b <= c

unjust (Just a) = a

bfs :: (Eq a) => (a -> [a]) -> [a] -> [a] -> [a]
bfs _ result [] = result
bfs neighbors result (node:queue) = bfs neighbors (node:result) (queue ++ (filter (flip notElem result) $ neighbors node))

hexToDec :: String -> Int
hexToDec = sum . zipWith (*) (iterate (* 16) 1) . reverse . map digitToInt
