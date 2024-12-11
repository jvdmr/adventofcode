module AoC2020
  ( days
  ) where

import AoC (Year)
import qualified AoC2020.Day01 as Day01
import qualified AoC2020.Day02 as Day02
import qualified AoC2020.Day03 as Day03
import qualified AoC2020.Day04 as Day04
import qualified AoC2020.Day05 as Day05
import qualified AoC2020.Day06 as Day06
import qualified AoC2020.Day07 as Day07
import qualified AoC2020.Day08 as Day08
import qualified AoC2020.Day09 as Day09
import qualified AoC2020.Day10 as Day10
import qualified AoC2020.Day11 as Day11
import qualified AoC2020.Day12 as Day12
import qualified AoC2020.Day13 as Day13
import qualified AoC2020.Day14 as Day14
import qualified AoC2020.Day15 as Day15
import qualified AoC2020.Day16 as Day16
import qualified AoC2020.Day17 as Day17
import qualified AoC2020.Day18 as Day18
import qualified AoC2020.Day19 as Day19
import qualified AoC2020.Day20 as Day20
import qualified AoC2020.Day21 as Day21
import qualified AoC2020.Day22 as Day22
import qualified AoC2020.Day23 as Day23
import qualified AoC2020.Day24 as Day24
import qualified AoC2020.Day25 as Day25

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
    , (13, (Day13.part1, Day13.part2))
    , (14, (Day14.part1, Day14.part2))
    , (15, (Day15.part1, Day15.part2))
    , (16, (Day16.part1, Day16.part2))
    , (17, (Day17.part1, Day17.part2))
    , (18, (Day18.part1, Day18.part2))
    , (19, (Day19.part1, Day19.part2))
    , (20, (Day20.part1, Day20.part2))
    , (21, (Day21.part1, Day21.part2))
    , (22, (Day22.part1, Day22.part2))
    , (23, (Day23.part1, Day23.part2))
    , (24, (Day24.part1, Day24.part2))
    , (25, (Day25.part1, Day25.part2))
    ]

