{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module Day02
  ( part1
  , part2
  ) where

import Data.List
import Data.List.Split (splitOn)
import Data.Map ((!))
import qualified Data.Map as M

import Vdmr.Generic

data Color = Red | Green | Blue
  deriving (Show, Eq, Ord)

readColor :: String -> Color
readColor "red" = Red
readColor "green" = Green
readColor "blue" = Blue

type CubeCount = (Color, Int)
parseCubeCount :: String -> CubeCount
parseCubeCount s = (readColor c, read n)
  where [n, c] = splitOn " " s

type Set = M.Map Color Int
parseSet :: String -> Set
parseSet s = M.fromList $ map parseCubeCount $ splitOn ", " s

type Game = (Int, [Set])
parseGame :: String -> Game
parseGame s = (read id, map parseSet $ splitOn "; " setstr)
  where [idstr, setstr] = splitOn ": " s 
        id = last $ splitOn " " idstr

possibleCubeCount :: Set -> CubeCount -> Bool
possibleCubeCount b (c, n) = (b ! c) >= n

possibleSet :: Set -> Set -> Bool
possibleSet b s = all (possibleCubeCount b) $ M.toList s

possibleGame :: Set -> Game -> Bool
possibleGame b (_, ss) = all (possibleSet b) ss

bag :: Set
bag = M.fromList [(Red, 12), (Green, 13), (Blue, 14)]

part1 :: Solver
part1 = show . sum . map fst . filter (possibleGame bag) . map parseGame . lines

insertMax :: Set -> Set -> Set
insertMax = M.unionWith max

emptyBag :: Set
emptyBag = M.fromList [(Red, 0), (Green, 0), (Blue, 0)]

leastCubes :: Game -> Set
leastCubes (_, ss) = foldl insertMax emptyBag ss

power :: Set -> Int
power s = foldl (*) 1 $ map snd $ M.toList s

part2 :: Solver
part2 = show . sum . map (power . leastCubes) . map parseGame . lines

