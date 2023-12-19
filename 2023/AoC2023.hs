module AoC2023
  ( days
  ) where

import Vdmr.Generic (Year)
import Day01
import Day02
import Day03
-- import Day04
-- import Day05
-- import Day06
-- import Day07
-- import Day08
-- import Day09
-- import Day10
-- import Day11
-- import Day12
-- import Day13
-- import Day14
-- import Day15
-- import Day16
-- import Day17
import Day18
import Day19
import Day20
import Day21
import Day22
import Day23
import Day24
import Day25

import Data.Map (fromList)

days :: Year
days =
  fromList
    [ (1, (Day01.part1, Day01.part2))
    , (2, (Day02.part1, Day02.part2))
    , (3, (Day03.part1, Day03.part2))
--     , (4, (Day04.part1, Day04.part2))
--     , (5, (Day05.part1, Day05.part2))
--     , (6, (Day06.part1, Day06.part2))
--     , (7, (Day07.part1, Day07.part2))
--     , (8, (Day08.part1, Day08.part2))
--     , (9, (Day09.part1, Day09.part2))
--     , (10, (Day10.part1, Day10.part2))
--     , (11, (Day11.part1, Day11.part2))
--     , (12, (Day12.part1, Day12.part2))
--     , (13, (Day13.part1, Day13.part2))
--     , (14, (Day14.part1, Day14.part2))
--     , (15, (Day15.part1, Day15.part2))
--     , (16, (Day16.part1, Day16.part2))
--     , (17, (Day17.part1, Day17.part2))
    , (18, (Day18.part1, Day18.part2))
    , (19, (Day19.part1, Day19.part2))
    , (20, (Day20.part1, Day20.part2))
    , (21, (Day21.part1, Day21.part2))
    , (22, (Day22.part1, Day22.part2))
    , (23, (Day23.part1, Day23.part2))
    , (24, (Day24.part1, Day24.part2))
    , (25, (Day25.part1, Day25.part2))
    ]

