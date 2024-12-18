{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC2024.Day05
  ( part1
  , part2
  , tests
  ) where

import Data.List.Split (splitOn)
import Data.List (sortBy)

import AoC (Solver, Tests)
import AoC.Util (pair)

type Page = Int
type Rules = (Page -> Page -> Ordering)
type Updates = [Page]

rulify :: [String] -> Rules
rulify ls = rules
  where rulelst = map (pair . map read . splitOn "|") ls
        rules a b | (a, b) `elem` rulelst = LT
                  | (b, a) `elem` rulelst = GT
                  | otherwise = EQ

updatify :: String -> [Page]
updatify = map read . splitOn ","

parseInput :: [String] -> (Rules, [Updates])
parseInput ls = (rulify a, map (updatify) b)
  where [a, b] = splitOn [""] ls

checkUpdates :: (Rules, [Updates]) -> [Updates]
checkUpdates (rules, updates) = map fst $ filter (uncurry (==)) $ zip updates $ map (sortBy rules) updates

middle :: Updates -> Int
middle lst = lst !! div (length lst) 2

tests :: Tests
tests =
  [ show . length . lines
  ]

part1 :: Solver
part1 = show . sum . map middle . checkUpdates . parseInput . lines

sortUpdates :: (Rules, [Updates]) -> [Updates]
sortUpdates (rules, updates) = map snd $ filter (uncurry (/=)) $ zip updates $ map (sortBy rules) updates

part2 :: Solver
part2 = show . sum . map middle . sortUpdates . parseInput . lines

