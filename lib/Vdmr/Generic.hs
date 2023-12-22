{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module Vdmr.Generic
  ( Solver (..)
  , Day (..)
  , Year (..)
  , idtrace
  , ftrace
  , flatten
  , uniq
  , between
  , unjust
  , bfs
  , bfsState
  , hexToDec 
  , none
  , andF
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

-- uniq is better than nub on sorted lists
uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq [a] = [a]
uniq (a:b:rst) | a == b = uniq (b:rst)
               | otherwise = a:uniq (b:rst)

between :: Int -> Int -> Int -> Bool
between a b c = a <= b && b <= c

unjust (Just a) = a

bfs :: (Eq a) => (a -> [a]) -> [a] -> [a] -> [a]
bfs _ result [] = result
bfs neighbors result (node:queue) = bfs neighbors (node:result) (queue ++ (filter (flip notElem result) $ neighbors node))

bfsState :: (Eq a) => b -> (b -> a -> (b, [a])) -> [a] -> [a] -> (b, [a])
bfsState state _ result [] = (state, result)
bfsState state neighbors result (node:queue) = bfsState state' neighbors (node:result) (queue ++ filter (flip notElem result) neighbors')
  where (state', neighbors') = neighbors state node

hexToDec :: String -> Int
hexToDec = sum . zipWith (*) (iterate (* 16) 1) . reverse . map digitToInt

none :: Foldable t => (a -> Bool) -> t a -> Bool
none f as = all (not . f) as

andF :: a -> [a -> Bool] -> Bool
andF x = and . map ($ x)
