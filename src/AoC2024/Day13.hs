{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC2024.Day13
  ( part1
  , part2
  , tests
  ) where

import Data.List.Split (splitOn)
import Data.Matrix
import Data.Either (fromRight)

import AoC (Solver, Tests)
import AoC.Util (toInt)
import AoC.Trace

type Coords = [Rational]

add :: Coords -> Coords -> Coords
add a b = zipWith (+) a b

coords :: String -> Coords
coords = map (toRational . read . drop 2) . splitOn ", " . head . tail . splitOn ": "

type ClawMachine = [Coords] -- [button A, button B, prize]

clawmachine :: [String] -> ClawMachine
clawmachine css = map coords css

col :: Int -> Matrix a -> [a]
col n = flip (!!) n . toLists . transpose

tokens :: ClawMachine -> Integer
tokens css = 3 * fr a + fr b
  where result@[a, b] = col 2 $ fromRight (zero 2 3) $ idtrace $ rref $ transpose $ fromLists css
        fr = fromRight 0 . toInt

tests :: Tests
tests =
  [ show . length . idtrace . splitOn [""] . lines
  , show . map (rtrace tokens . patchmachine 10000000000000 . clawmachine) . splitOn [""] . lines
  , show . length . filter (> 0) . map (tokens . patchmachine 10000000000000 . clawmachine) . splitOn [""] . lines
  ]

part1 :: Solver
part1 = show . sum . map (tokens . clawmachine) . splitOn [""] . lines

patchmachine :: Rational -> ClawMachine -> ClawMachine
patchmachine i [a, b, p] = [a, b, add (repeat i) p]

part2 :: Solver
part2 = show . sum . map (tokens . patchmachine 10000000000000 . clawmachine) . splitOn [""] . lines

