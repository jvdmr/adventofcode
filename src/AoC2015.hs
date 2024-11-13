module AoC2015
  ( days
  ) where

import Vdmr.Generic (Year)
import AoC2015.Day01 as Day01
import AoC2015.Day02 as Day02
import AoC2015.Day03 as Day03
import AoC2015.Day04 as Day04
import AoC2015.Day05 as Day05
import AoC2015.Day06 as Day06
import AoC2015.Day07 as Day07
import AoC2015.Day08 as Day08
import AoC2015.Day09 as Day09
import AoC2015.Day10 as Day10
import AoC2015.Day11 as Day11
import AoC2015.Day12 as Day12
import AoC2015.Day13 as Day13
import AoC2015.Day14 as Day14
import AoC2015.Day15 as Day15
import AoC2015.Day16 as Day16
import AoC2015.Day17 as Day17
import AoC2015.Day18 as Day18
import AoC2015.Day19 as Day19
import AoC2015.Day20 as Day20
import AoC2015.Day21 as Day21
import AoC2015.Day22 as Day22
import AoC2015.Day23 as Day23
import AoC2015.Day24 as Day24
import AoC2015.Day25 as Day25

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

