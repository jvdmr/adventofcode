{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC2023.Day25
  ( part1
  , part2
  ) where

import Data.List
import Data.List.Split (splitOn)
import Data.Map ((!))
import qualified Data.Map as M

import Vdmr.Generic

type Label = String
type Link = (Label, [Label])
type Graph = M.Map Label [Label]

link :: String -> Link
link s = (a, splitOn " " b)
  where [a, b] = splitOn ": " s

switch :: Link -> [Link]
switch (a, bs) = zip bs $ repeat [a]

merge :: [Link] -> Link
merge ls = (fst $ head ls, concat $ map snd ls)

fromLinks :: [Link] -> Graph
fromLinks ls = M.fromList $ map merge $ groupOn fst $ sortOn fst $ ls ++ ls'
  where ls' = concat $ map switch ls

part1 :: Solver
part1 = show . length . M.toList . fromLinks . map link . lines

part2 :: Solver
part2 = show . length . map link . lines

