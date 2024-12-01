{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC.Util
  ( andF
  , between
  , bfs
  , bfsState
  , bfsTiers
  , cartesian
  , cartesianInf
  , cartesianInfWith
  , cartesianWith
  , count
  , countablePairs
  , countableZPairs
  , equating
  , groupOn
  , hexToDec 
  , iterateUntilIdempotent 
  , longerThan
  , none
  , pascal
  , strings
  , uniq
  , unjust
  ) where

import Data.List (groupBy, inits, nub)
import Data.Char (digitToInt)

-- uniq is better than nub on sorted lists
uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq [a] = [a]
uniq (a:b:rst) | a == b = uniq (b:rst)
               | otherwise = a:uniq (b:rst)

equating :: (Eq b) => (a -> b) -> a -> a -> Bool
equating f a b = f a == f b

count :: (a -> Bool) -> [a] -> Int
count pred = length . filter pred

between :: Ord a => a -> a -> a -> Bool
between a b c = a <= b && b <= c

unjust :: Maybe a -> a
unjust (Just a) = a

bfs :: (Eq a) => (a -> [a]) -> a -> [(a, Int)]
bfs neighbors start = bfs' [start] [(start, 0)]
  where bfs' _ [] = []
        bfs' seen (node@(n, d):queue) = node:let nbs = filter (flip notElem seen) $ neighbors n in bfs' (nbs ++ seen) $ queue ++ map (flip (,) (d + 1)) nbs

bfsTiers :: (Eq a) => (a -> [a]) -> a -> [([a], Int)]
bfsTiers neighbors start = bfs' ([start], []) ([start], 0)
  where bfs' _ ([], _) = []
        bfs' (cur, prev) nodes@(ns, d) = nodes:let nbs = filter (flip notElem prev) $ nub $ concat $ map neighbors ns in bfs' (nbs, cur) (nbs, d + 1)

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

strings :: [String]
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

-- infinite list of pairs of natural numbers, starting at (0, 0)
countablePairs :: [(Int, Int)]
countablePairs = cp 0 0
  where cp 0 j = (0, j):cp (j + 1) 0
        cp i j = (i, j):cp (i - 1) (j + 1)

-- infinite list of pairs of whole numbers (including negatives), starting at (0, 0)
countableZPairs :: [(Int, Int)]
countableZPairs = cp 0 0
  where cp 0 j = (0, j):(0, -j):cp (j + 1) 0
        cp i 0 = (i, 0):(-i, 0):cp (i - 1) 1
        cp i j = (i, j):(-i, j):(i, -j):(-i, -j):cp (i - 1) (j + 1)

-- combine two infinite lists in all possible combinations (1-dimensional grid)
cartesianInfWith :: (a -> b -> c) -> [a] -> [b] -> [c]
cartesianInfWith f a b = [f (a !! i) (b !! j) | (i, j) <- countablePairs]

cartesianInf :: [a] -> [b] -> [(a, b)]
cartesianInf = cartesianInfWith (,)
