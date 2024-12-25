{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC.Util
  ( Pair (..)
  , Binary (..)
  , Bit (..)
  , ($<)
  , ($>)
  , add
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
  , combine
  , combine3
  , count
  , countablePairs
  , countableZPairs
  , equating
  , groupOn
  , head'
  , hexToDec 
  , intToBin
  , iterateUntilIdempotent 
  , last'
  , longerThan
  , multiply
  , neg
  , none
  , orF
  , pair
  , pascal
  , primes
  , readChar
  , shortestLists
  , skipOne
  , stopLoop
  , strings
  , takeUntil
  , toInt
  , toList
  , trueOrFalse
  , uncurryL
  , uniq
  , unpair
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

-- uniq is better than nub on sorted lists
uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq [a] = [a]
uniq (a:b:rst) | a == b = uniq (b:rst)
               | otherwise = a:uniq (b:rst)

head' :: [a] -> [a]
head' [] = []
head' a = [head a]

last' :: [[a]] -> [a]
last' [] = []
last' a = last a

equating :: (Eq b) => (a -> b) -> a -> a -> Bool
equating f a b = f a == f b

count :: (a -> Bool) -> [a] -> Int
count pred = length . filter pred

between :: Ord a => a -> a -> a -> Bool
between a c b = a <= b && b <= c

hexToDec :: String -> Int
hexToDec = sum . zipWith (*) (iterate (* 16) 1) . reverse . map digitToInt

none :: Foldable t => (a -> Bool) -> t a -> Bool
none f as = all (not . f) as

andF :: a -> [a -> Bool] -> Bool
andF x = and . map ($ x)

orF :: a -> [a -> Bool] -> Bool
orF x = or . map ($ x)

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

primes :: [Int]
primes = sieve [2..]
  where sieve (p:xs) = p:sieve [x | x <- xs, mod x p > 0]

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

-- takeUntil is takeWhile plus one extra element
takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil f (a:as) | f a = a:takeUntil f as
                   | otherwise = [a]

stopLoop :: (Eq a) => [a] -> [a]
stopLoop [] = []
stopLoop (a:as) = a:takeWhile (/= a) as

readChar :: (Read a) => Char -> a
readChar c = read [c]

toInt :: Integral a => Ratio a -> Either a a -- Left for rounded result, Right when a is already whole
toInt a | n `mod` d == 0 = Right $ n `div` d
        | otherwise = Left $ round $ (fromIntegral n / fromIntegral d)
  where n = numerator a
        d = denominator a

type Pair a = (a, a)

toList :: Pair a -> [a]
toList (a, b) = [a, b]

pair :: [a] -> Pair a
pair [a, b] = (a, b)

unpair :: (a -> b) -> Pair a -> Pair b
unpair f (a, b) = (f a, f b)

combine :: (a -> b -> c) -> Pair a -> Pair b -> Pair c
combine f (a, b) (c, d) = (f a c, f b d)

combine3 :: (a -> b -> c -> d) -> Pair a -> Pair b -> Pair c -> Pair d
combine3 f a = combine ($) . combine f a

add :: Num a => Pair a -> Pair a -> Pair a
add = combine (+)

neg :: Num a => Pair a -> Pair a
neg (a, b) = (-a, -b)

multiply :: Num a => a -> Pair a -> Pair a
multiply n = combine (*) (n, n)

($<) :: (a -> c) -> (a, b) -> (c, b)
($<) f (a, b) = (f a, b)

($>) :: (b -> c) -> (a, b) -> (a, c)
($>) f (a, b) = (a, f b)

trueOrFalse :: (a -> Bool) -> [a] -> Pair [a]
trueOrFalse pred = unpair reverse . foldl' f ([], [])
  where f (ts, fs) v | pred v = (v:ts, fs)
                     | otherwise = (ts, v:fs)

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

shortestLists :: (Eq a, Ord a) => [[a]] -> [[a]]
shortestLists = head . groupBy (equating length) . sortBy (comparing length)
