{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC2025.Day10
  ( part1
  , part2
  , tests
  ) where

import Prelude hiding (foldl) -- use foldl' instead of foldl

import AoC (Solver, Tests)
import AoC.Bfs (bfs, bfsAdvanced, NeighborFunction, UnseenFilter)
import AoC.Text (csl)

type BlinkenLights = [Bool]
type Button = [Int]
type Buttons = [Button]
type Joltages = [Int]

data Machine = Machine
  { expectedState::BlinkenLights
  , state::BlinkenLights
  , buttons::Buttons
  , expectedJoltages::Joltages
  , joltages::Joltages
  }
  deriving (Show, Eq)

-- just cut off the first and last element/char
unwrap :: [a] -> [a]
unwrap (_:rst) = init rst

parseBlinkenLights :: String -> BlinkenLights
parseBlinkenLights = map (== '#') . unwrap

parseButton :: String -> Button
parseButton = map read . csl . unwrap

parseButtons :: [String] -> Buttons
parseButtons = map parseButton

parseJoltages :: String -> Joltages
parseJoltages = map read . csl . unwrap

machine :: [String] -> Machine
machine machineDesc = Machine
  { expectedState = es
  , state = take (length es) $ repeat False
  , buttons = parseButtons $ init $ tail machineDesc
  , expectedJoltages = js
  , joltages = take (length js) $ repeat 0
  }
  where es = parseBlinkenLights $ head machineDesc  -- expected state
        js = parseJoltages $ last machineDesc

type NextStatesFunction = NeighborFunction Machine

flipLight :: BlinkenLights -> Int -> BlinkenLights
flipLight (l:ls) 0 = (not l):ls
flipLight (l:ls) n = l:flipLight ls (n - 1)

pressLightsButton :: Machine -> Button -> Machine
pressLightsButton m b = m { state = (foldl' flipLight (state m) b) }

started :: Machine -> Bool
started m = state m == expectedState m

nextLightStates :: NextStatesFunction
nextLightStates machine = map (pressLightsButton machine) $ buttons machine

startMachine :: Machine -> Int
startMachine = snd . head . dropWhile (not . started . fst) . bfs nextLightStates

part1 :: Solver
part1 = show . sum . map (startMachine . machine . words) . lines

part2 :: Solver
part2 = ("Not yet solved! " ++) . show . length . lines

tests :: Tests
tests =
  [ show . length . lines
  ]

