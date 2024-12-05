{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC.Util
  ( andF
  , between
  , cartesian
  , cartesianInf
  , cartesianInfWith
  , cartesianWith
  , count
  , countablePairs
  , countableZPairs
  , equating
  , groupOn
  , head'
  , hexToDec 
  , iterateUntilIdempotent 
  , last'
  , longerThan
  , none
  , pair
  , pascal
  , skipOne
  , strings
  , uncurryL
  , uniq
  , unjust
  , zipTail
  , zipTailWith
  ) where

import Data.List (groupBy, inits)
import Data.Char (digitToInt)

-- uniq is better than nub on sorted lists
uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq [a] = [a]
uniq (a:b:rst) | a == b = uniq (b:rst)
               | otherwise = a:uniq (b:rst)

head' :: [[a]] -> [a]
head' [] = []
head' a = head a

last' :: [[a]] -> [a]
last' [] = []
last' a = last a

equating :: (Eq b) => (a -> b) -> a -> a -> Bool
equating f a b = f a == f b

count :: (a -> Bool) -> [a] -> Int
count pred = length . filter pred

between :: Ord a => a -> a -> a -> Bool
between a c b = a <= b && b <= c

unjust :: Maybe a -> a
unjust (Just a) = a

hexToDec :: String -> Int
hexToDec = sum . zipWith (*) (iterate (* 16) 1) . reverse . map digitToInt

none :: Foldable t => (a -> Bool) -> t a -> Bool
none f as = all (not . f) as

andF :: a -> [a -> Bool] -> Bool
andF x = and . map ($ x)

strings :: [String]
strings = a ++ [f a' | f <- map (++) strings, a' <- a]
  where a = map (:[]) ['A'..'Z']

skipOne :: [a] -> [[a]]
skipOne [] = [[]]
skipOne (a:as) = as:map (a:) (skipOne as)

zipTailWith :: (a -> a -> b) -> [a] -> [b]
zipTailWith f a = zipWith f a $ tail a

zipTail :: [a] -> [(a, a)]
zipTail = zipTailWith (,)

iterateUntilIdempotent :: Eq a => (a -> a) -> a -> [a]
iterateUntilIdempotent f x = x:(map snd $ takeWhile (uncurry (/=)) $ zipTail $ iterate f x)

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

uncurryL :: (a -> a -> b) -> [a] -> b
uncurryL f (a:b:_) = f a b

pair :: [a] -> (a, a)
pair [a, b] = (a, b)

