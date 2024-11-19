{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module Vdmr.Generic
  ( Solver (..)
  , Day (..)
  , Year (..)
  , uniq
  , between
  , unjust
  , bfs
  , bfsState
  , hexToDec 
  , none
  , andF
  , strings
  , iterateUntilIdempotent 
  , groupOn
  ) where

import Data.List (groupBy)
import Data.Char (digitToInt)
import Data.Map (Map)

type Solver = (String -> String)
type Day = (Solver, Solver)
type Year = Map Int Day

-- uniq is better than nub on sorted lists
uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq [a] = [a]
uniq (a:b:rst) | a == b = uniq (b:rst)
               | otherwise = a:uniq (b:rst)

between :: Ord a => a -> a -> a -> Bool
between a b c = a <= b && b <= c

unjust (Just a) = a

bfs :: (Eq a) => (a -> [a]) -> ([a] -> [a]) -> [a] -> [a] -> [a]
bfs _ _ result [] = result
bfs neighbors prune result (node:queue) = bfs neighbors prune (node:result) $ prune (queue ++ (filter (flip notElem (queue ++ result)) $ neighbors node))

bfsState :: (Eq a) => b -> (b -> a -> (b, [a])) -> ([a] -> [a]) -> [a] -> [a] -> (b, [a])
bfsState state _ _ result [] = (state, result)
bfsState state neighbors prune result (node:queue) = bfsState state' neighbors prune (node:result) $ prune (queue ++ filter (flip notElem (queue ++ result)) neighbors')
  where (state', neighbors') = neighbors state node

hexToDec :: String -> Int
hexToDec = sum . zipWith (*) (iterate (* 16) 1) . reverse . map digitToInt

none :: Foldable t => (a -> Bool) -> t a -> Bool
none f as = all (not . f) as

andF :: a -> [a -> Bool] -> Bool
andF x = and . map ($ x)

strings = a ++ [f a' | f <- map (++) strings, a' <- a]
  where a = map (:[]) ['A'..'Z']

iterateUntilIdempotent :: Eq a => (a -> a) -> a -> [a]
iterateUntilIdempotent f x = x:(map snd $ takeWhile (uncurry (/=)) $ zip results $ tail results)
  where results = iterate f x

groupOn :: Eq a => (b -> a) -> [b] -> [[b]]
groupOn f lst = map (map fst) $ groupBy eq $ zip lst $ map f lst
  where eq (_, a) (_, b) = a == b

