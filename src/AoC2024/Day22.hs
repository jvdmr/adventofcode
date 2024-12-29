{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC2024.Day22
  ( part1
  , part2
  , tests
  ) where

import Prelude hiding (foldl)
import Data.List (groupBy, tails, sortBy, nubBy)
import Data.Ord (comparing)

import AoC (Solver, Tests)
import AoC.Util (xor, xorbits, intToBin, binToInt, Bit(..), Binary(..), bits, equating)

type RBits = [Bit]

rbits :: Int -> RBits
rbits = reverse . bits . intToBin

fromRBits :: RBits -> Int
fromRBits = binToInt . Binary . reverse

mixprune :: RBits -> RBits -> RBits
mixprune n r | length n > length r = take 24 $ xorbits n (r ++ repeat Zero)
             | otherwise = take 24 $ xorbits (n ++ repeat Zero) r

fa :: RBits -> RBits
fa bn = mixprune bn $ (take 6 $ repeat Zero) ++ bn

fb :: RBits -> RBits
fb bn = mixprune bn $ drop 5 bn

fc :: RBits -> RBits
fc bn = mixprune bn $ (take 11 $ repeat Zero) ++ bn

nextSecret :: RBits -> RBits
nextSecret = fc . fb . fa

ns :: Int -> Int -> Int
ns n = fromRBits . (!!) ns'
  where ns' = iterate nextSecret $ rbits n

tests :: Tests
tests =
  [ show . length . lines
  , const $ show $ xor 42 15
  , const $ show $ map (ns 123) [1..10]
  , const $ show $ ns 123 2000
  ]

part1 :: Solver
part1 = show . sum . map (flip ns 2000 . read) . lines

prices :: [Int] -> [[Int]]
prices = map (map (flip mod 10 . fromRBits) . take 2000 . iterate nextSecret . rbits)

pattern :: [Int] -> ([Int], Int)
pattern l = (zipWith (-) (tail l) l, last l)

patterns :: [Int] -> [([Int], Int)]
patterns = nubBy (equating fst) . map (pattern . take 5) . takeWhile (flip (>) 4 . length) . tails

bananas :: [[Int]] -> Int
bananas = maximum . map (sum . map snd) . groupBy (equating fst) . sortBy (comparing fst) . concat . map patterns

part2 :: Solver
part2 = show . bananas . prices . map read . lines

