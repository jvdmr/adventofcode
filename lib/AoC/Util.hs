{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC.Util
  ( Binary (..)
  , Bit (..)
  , ($<)
  , ($>)
  , andF
  , between
  , binToInt
  , bits
  , boolToBin
  , boolToBit
  , bxor
  , cartesian
  , cartesianInf
  , cartesianInfWith
  , cartesianWith
  , col
  , count
  , countablePairs
  , countableZPairs
  , equating
  , filterFirst
  , groupOn
  , head'
  , hexToDec 
  , ignoreInput
  , intToBin
  , iterateUntilIdempotent 
  , last'
  , longerThan
  , multiply
  , none
  , orF
  , pascal
  , primes
  , readChar
  , shortestLists
  , skipOne
  , stopLoop
  , strings
  , takeUntil
  , toInt
  , trueOrFalse
  , uncurryL
  , uniq
  , xor
  , xorbits
  , zipTail
  , zipTailWith
  ) where

import Data.List (sortBy, groupBy, inits)
import Data.Ord (comparing)
import Data.Char (digitToInt)
import Data.Ratio (Ratio, numerator, denominator)
import Data.Matrix (Matrix, toLists, transpose)
import Data.Function (on)

import AoC (Solver)
import AoC.Pair (Pair(..), unpair)

-- uniq is better than nub on sorted lists
uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq [a] = [a]
uniq (a:b:rst) | a == b = uniq (b:rst)
               | otherwise = a:uniq (b:rst)

-- like head, but returns a list with only the first element, and an empty list if the given list is empty
head' :: [a] -> [a]
head' [] = []
head' a = [head a]

-- like last, but returns a list with only the last element, and an empty list if the given list is empty
last' :: [[a]] -> [a]
last' [] = []
last' a = last a

-- like comparing, only checks if results of given function are equal
equating :: (Eq b) => (a -> b) -> a -> a -> Bool
equating = on (==)

-- count how many elements in list match predicate
count :: (a -> Bool) -> [a] -> Int
count pred = length . filter pred

-- check if third arg is between first and second, including
between :: Ord a => a -> a -> a -> Bool
between a c b = a <= b && b <= c

-- convert hexadecimal string to int
hexToDec :: String -> Int
hexToDec = sum . zipWith (*) (iterate (* 16) 1) . reverse . map digitToInt

-- like all, but opposite
none :: Foldable t => (a -> Bool) -> t a -> Bool
none f as = all (not . f) as

-- like all, but with 1 arg and a list of predicates
andF :: a -> [a -> Bool] -> Bool
andF x = and . map ($ x)

-- like any, but with 1 arg and a list of predicates
orF :: a -> [a -> Bool] -> Bool
orF x = or . map ($ x)

-- generate infinite list of (all-caps) strings, eg. ["A", "B", "C", ..., "Z", "AA", "AB", "AC", ..., "ZY", "ZZ", "AAA", ...]
strings :: [String]
strings = a ++ [f a' | f <- map (++) strings, a' <- a]
  where a = map (:[]) ['A'..'Z']

-- Create sublists where every element in turn is skipped (and full list included as last element)
skipOne :: [a] -> [[a]]
skipOne [] = [[]]
skipOne (a:as) = as:map (a:) (skipOne as)

-- Zip a list with its own tail using given function
zipTailWith :: (a -> a -> b) -> [a] -> [b]
zipTailWith f a = zipWith f a $ tail a

-- Zip a list with its own tail
zipTail :: [a] -> [(a, a)]
zipTail = zipTailWith (,)

-- Like iterate, but stop when the same result appears twice in a row
iterateUntilIdempotent :: Eq a => (a -> a) -> a -> [a]
iterateUntilIdempotent f x = x:(map snd $ takeWhile (uncurry (/=)) $ zipTail $ iterate f x)

-- Group elements of a list based on predicate
groupOn :: Eq a => (b -> a) -> [b] -> [[b]]
groupOn f lst = map (map fst) $ groupBy eq $ zip lst $ map f lst
  where eq (_, a) (_, b) = a == b

-- Pascal's triangle - line n, position m
pascal :: Int -> Int -> Int
pascal n m = (triangle !! (m - 1)) !! (n - 1)
  where triangle = (repeat 1):map (map sum . inits) triangle

-- Infinite list of prime numbers
primes :: [Int]
primes = sieve [2..]
  where sieve (p:xs) = p:sieve [x | x <- xs, mod x p > 0]

-- combine two finite lists in all possible combinations
cartesianWith :: (a -> b -> c) -> [a] -> [b] -> [c]
cartesianWith f a b = [f (a !! i) (b !! j) | i <- [0..x], j <- [0..y]]
  where x = length a - 1
        y = length b - 1

-- combine two finite lists into pairs
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

-- combine two infinite lists in all possible combinations (1-dimensional grid) as pairs
cartesianInf :: [a] -> [b] -> [(a, b)]
cartesianInf = cartesianInfWith (,)

-- Like uncurry, but on (the first two elements of) a list instead of a tuple
uncurryL :: (a -> a -> b) -> [a] -> b
uncurryL f (a:b:_) = f a b

-- takeUntil is takeWhile plus one extra element
takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil f (a:as) | f a = a:takeUntil f as
                   | otherwise = [a]

-- first part of a list until the first element is repeated
stopLoop :: (Eq a) => [a] -> [a]
stopLoop [] = []
stopLoop (a:as) = a:takeWhile (/= a) as

-- read a single char
readChar :: (Read a) => Char -> a
readChar c = read [c]

-- convert Ratio to Integral
toInt :: Integral a => Ratio a -> Either a a -- Left for rounded result, Right when a is already whole
toInt a | n `mod` d == 0 = Right $ n `div` d
        | otherwise = Left $ round $ (fromIntegral n / fromIntegral d)
  where n = numerator a
        d = denominator a

-- apply function to fst of tuple
($<) :: (a -> c) -> (a, b) -> (c, b)
($<) f (a, b) = (f a, b)

-- apply function to snd of tuple
($>) :: (b -> c) -> (a, b) -> (a, c)
($>) f (a, b) = (a, f b)

-- Split a list into elements matching predicates, and those that don't
trueOrFalse :: (a -> Bool) -> [a] -> Pair [a]
trueOrFalse pred = unpair reverse . foldl' f ([], [])
  where f (ts, fs) v | pred v = (v:ts, fs)
                     | otherwise = (ts, v:fs)

-- Get column n out of a Matrix
col :: Int -> Matrix a -> [a]
col n = flip (!!) n . toLists . transpose

data Bit = Zero | One
  deriving (Eq, Ord, Enum)

instance Show Bit where
  show Zero = "0"
  show One = "1"

boolToBit :: Bool -> Bit
boolToBit b | b = One
            | otherwise = Zero

data Binary = Binary [Bit]
  deriving (Eq)

bits :: Binary -> [Bit]
bits (Binary bs) = bs

binToInt :: Binary -> Int
binToInt = bti 0 . bits
  where bti v [] = v
        bti v (Zero:bs) = bti (2*v) bs
        bti v (One:bs) = bti (2*v + 1) bs

boolToBin :: [Bool] -> Binary
boolToBin = Binary . map boolToBit

intToBin :: Int -> Binary
intToBin n = Binary $ itb n []
  where itb 0 bs = Zero:bs
        itb 1 bs = One:bs
        itb i bs | mod i 2 == 0 = itb (div i 2) $ Zero:bs
                 | otherwise = itb (div i 2) $ One:bs

instance Num Binary where
  negate a = a -- no negative binary numbers
  (+) a b = intToBin $ (binToInt a) + (binToInt b)
  (*) a b = intToBin $ (binToInt a) * (binToInt b)
  fromInteger a = intToBin $ fromInteger a
  abs = id -- no negative numbers
  signum _ = 1 -- no negative numbers

instance Ord Binary where
  compare = comparing binToInt

instance Show Binary where
  show (Binary bs) = (++) "0b" $ concat $ map show bs

xorbits :: [Bit] -> [Bit] -> [Bit]
xorbits [] bs = []
xorbits as [] = []
xorbits (a:as) (b:bs) | a == b = Zero:xorbits as bs
                      | otherwise = One:xorbits as bs

bxor :: Binary -> Binary -> Binary
bxor (Binary as) (Binary bs) = Binary $ xorbits as' bs'
  where las = length as
        lbs = length bs
        as' = if las < lbs then (take (lbs - las) $ repeat Zero) ++ as else as
        bs' = if lbs < las then (take (las - lbs) $ repeat Zero) ++ bs else bs

xor :: Int -> Int -> Int
xor a b = binToInt $ bxor (intToBin a) (intToBin b)

-- get only the shortest (equal length) lists out of a list of lists
shortestLists :: (Eq a, Ord a) => [[a]] -> [[a]]
shortestLists = head . groupBy (equating length) . sortBy (comparing length)

-- Used for testing while ignoring the input
ignoreInput :: String -> Solver
ignoreInput s _ = s

-- like sum, calculate the product of a list of factors
multiply :: Num a => [a] -> a
multiply = foldl' (*) 1

-- remove the first matching element from a list
filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst f (a:as) | f a = as
                     | otherwise = a:filterFirst f as

