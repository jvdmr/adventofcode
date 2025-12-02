module AoC2025
  ( days
  , tests
  ) where

import AoC (Year, YearTests)
import qualified AoC2025.Day01 as Day01
import qualified AoC2025.Day02 as Day02
import qualified AoC2025.Day03 as Day03
import qualified AoC2025.Day04 as Day04
import qualified AoC2025.Day05 as Day05
import qualified AoC2025.Day06 as Day06
import qualified AoC2025.Day07 as Day07
import qualified AoC2025.Day08 as Day08
import qualified AoC2025.Day09 as Day09
import qualified AoC2025.Day10 as Day10
import qualified AoC2025.Day11 as Day11
import qualified AoC2025.Day12 as Day12

import Data.Map (fromList)

days :: Year
days =
  fromList
    [ (1, (Day01.part1, Day01.part2))
    , (2, (Day02.part1, Day02.part2))
    , (3, (Day03.part1, Day03.part2))
    , (4, (Day04.part1, Day04.part2))
    , (5, (Day05.part1, Day05.part2))
    , (6, (Day06.part1, Day06.part2))
    , (7, (Day07.part1, Day07.part2))
    , (8, (Day08.part1, Day08.part2))
    , (9, (Day09.part1, Day09.part2))
    , (10, (Day10.part1, Day10.part2))
    , (11, (Day11.part1, Day11.part2))
    , (12, (Day12.part1, Day12.part2))
    ]

tests :: YearTests
tests =
  fromList
    [ (1, Day01.tests)
    , (2, Day02.tests)
    , (3, Day03.tests)
    , (4, Day04.tests)
    , (5, Day05.tests)
    , (6, Day06.tests)
    , (7, Day07.tests)
    , (8, Day08.tests)
    , (9, Day09.tests)
    , (10, Day10.tests)
    , (11, Day11.tests)
    , (12, Day12.tests)
    ]

