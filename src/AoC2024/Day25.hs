{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC2024.Day25
  ( part1
  , part2
  , tests
  ) where

import Prelude hiding (foldl)
import Data.List (transpose)
import Data.List.Split (splitOn)

import AoC (Solver, Tests)
import AoC.Util (trueOrFalse)

data Lock = Lock Int [Int]
          | Key Int [Int]
  deriving (Eq, Show)

key :: Lock -> Bool
key (Key _ _) = True
key _ = False

makeLock :: [String] -> Lock
makeLock (('#':_):l) = let l' = transpose l
                           in Lock (flip (-) 1 $ length $ head l') $ map (length . takeWhile ((==) '#')) l'
makeLock (('.':_):l) = let l' = transpose l
                           sz = (flip (-) 1 $ length $ head l')
                           in Key sz $ map ((-) sz . length . takeWhile ((==) '.')) l'

parseLocks :: [[String]] -> [Lock]
parseLocks = map makeLock

fits :: Lock -> Lock -> Bool
fits (Lock lsz pins) (Key ksz bumps) | lsz == ksz = null $ filter (flip (>) lsz) $ zipWith (+) pins bumps
                                     | otherwise = False
fits _ _ = False

matchLocks :: [Lock] -> [Lock] -> [(Lock, Lock)]
matchLocks keys locks = filter (uncurry fits) [(l, k) | k <- keys, l <- locks]

tests :: Tests
tests =
  [ show . length . lines
  , show . parseLocks . splitOn [""] . lines
  ]

part1 :: Solver
part1 = show . length . uncurry matchLocks . trueOrFalse key . parseLocks . splitOn [""] . lines

part2 :: Solver
part2 = ("Not yet solved! " ++) . show . length . lines

