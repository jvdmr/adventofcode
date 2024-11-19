{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Vdmr.Generic (Year)

import AoC2015
import AoC2016
import AoC2020
import AoC2021
import AoC2022
import AoC2023
import AoC2024

import Data.List.Split (splitOn)
import Data.Map (Map, fromList, (!))
import System.Console.CmdArgs.Implicit (Data, Typeable, args, cmdArgs, def, help, opt, (&=))

data Arguments = Arguments { yearstr :: String, daystr :: String, part :: Int }
  deriving (Show, Data, Typeable)

years :: Map Int Year
years =
  fromList
    [ (2015, AoC2015.days)
    , (2016, AoC2016.days)
    , (2020, AoC2020.days)
    , (2021, AoC2021.days)
    , (2022, AoC2022.days)
    , (2023, AoC2023.days)
    , (2024, AoC2024.days)
    ]

main :: IO ()
main = do
  let arguments =
        Arguments
          { yearstr = def &= help "Which year to process"
          , daystr = def &= help "Which day of the year to process"
          , part = def &= help "Which part to run"
          }
  args <- cmdArgs arguments
  let theYear = read $ yearstr args
      theDay = read $ daystr args
      (solve1, solve2) = years ! theYear ! theDay
  input <- getContents
  case part args of
       1 -> do
         putStrLn $ "Part 1: " ++ solve1 input
       2 -> do
         putStrLn $ "Part 2: " ++ solve2 input

