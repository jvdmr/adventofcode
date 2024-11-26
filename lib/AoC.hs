{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC
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
  , pascal
  , cartesianWith
  , cartesian
  , longerThan
  , countablePairs
  , cartesianInfWith
  , cartesianInf
  ) where

import Data.List (groupBy, inits, sortBy, (!!))
import Data.Ord (comparing)
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

-- Pascal's triangle - line n, position m
pascal :: Int -> Int -> Int
pascal n m = (triangle !! (m - 1)) !! (n - 1)
  where triangle = (repeat 1):map (map sum . inits) triangle

-- combine two finite lists in all possible combinations
cartesianWith :: (a -> b -> c) -> [a] -> [b] -> [c]
cartesianWith f a b = [f (a !! i) (b !! j) | i <- [0..x], j <- [0..y]]
  where x = length a - 1
        y = length b - 1

cartesian :: [a] -> [b] -> [(a, b)]
cartesian = cartesianWith (,)

-- test if a list is longer than x
longerThan :: [a] -> Int -> Bool
longerThan l n = not $ null $ drop n l

-- infinite list of pairs of numbers, starting at (0, 0)
countablePairs :: [(Int, Int)]
countablePairs = cp 0 0
  where cp 0 j = (0, j):cp (j + 1) 0
        cp i j = (i, j):cp (i - 1) (j + 1)

-- combine two infinite lists in all possible combinations (1-dimensional grid)
cartesianInfWith :: (a -> b -> c) -> [a] -> [b] -> [c]
cartesianInfWith f a b = [f (a !! i) (b !! j) | (i, j) <- countablePairs]

cartesianInf :: [a] -> [b] -> [(a, b)]
cartesianInf = cartesianInfWith (,)

