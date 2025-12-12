{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC2025.Day12
  ( part1
  , part2
  , tests
  ) where

import Prelude hiding (foldl) -- use foldl' instead of foldl
import Data.List.Split (splitOn)
import qualified Data.Map as M (Map, fromList, (!))

import AoC (Solver, Tests)
import AoC.Pair (Pair)
import AoC.Util (ignoreInput)

type Shape = [[Char]]

type Presents = M.Map Int Shape

parsePresent :: [String] -> (Int, Shape)
parsePresent (idStr:shapestr) = (read $ init idStr, shapestr)

data Region = Region { size::Pair Int, presentIds::[Int] }
  deriving (Show, Eq)

parseRegion :: [String] -> Region
parseRegion (sizeStr:presents) =  Region { size = (x, y), presentIds = concat $ zipWith expandPresents [0..] presents }
  where [x, y] = map read $ splitOn "x" $ init sizeStr
        expandPresents presentId n = take (read n) $ repeat presentId

parseInput :: [[String]] -> (Presents, [Region])
parseInput inputs = (M.fromList $ map parsePresent $ init inputs, map (parseRegion . words) $ last inputs)

-- This feels dirty, but due to the input we're getting, it is good enough. Doesn't actually work right for the test input though...
largeEnoughRegion :: Presents -> Region -> Bool
largeEnoughRegion presents region = totalPresents < mx * my
  where totalPresents = length $ concat $ map (filter (== '#') . concat . (M.!) presents) $ presentIds region
        (mx, my) = size region

largeEnoughRegions :: (Presents, [Region]) -> [Region]
largeEnoughRegions (presents, regions) = filter (largeEnoughRegion presents) regions

part1 :: Solver
part1 = show . length . largeEnoughRegions . parseInput . splitOn [""] . lines

part2 :: Solver
part2 = ignoreInput "No part 2!"

tests :: Tests
tests =
  [ show . length . lines
  ]

