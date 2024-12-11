{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import AoC (Year, YearTests, Tests)

import qualified AoC2015
import qualified AoC2016
import qualified AoC2020
import qualified AoC2021
import qualified AoC2022
import qualified AoC2023
import qualified AoC2024
-- TEMPLATE import qualified AoCxxxx

import Data.Map (Map, fromList, (!))
import System.Console.CmdArgs.Implicit (Data, Typeable, cmdArgs, def, help, (&=))

data Arguments = Arguments { yearstr :: String, daystr :: String, part :: String }
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
    -- TEMPLATE , (xxxx, AoCxxxx.days)
    ]

yeartests :: Map Int YearTests
yeartests =
  fromList
    [ (2024, AoC2024.tests)
    -- TEMPLATE , (xxxx, AoCxxxx.tests)
    ]

runtests :: Tests -> String -> String
runtests tests input = concat $ map runtest $ zip [1..] tests
  where runtest (i, t) = "\n\nTest " ++ show i ++ ":\n" ++ t input

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
      tests = yeartests ! theYear ! theDay
  input <- getContents
  case part args of
       "1" -> do
         putStrLn $ "Part 1: " ++ solve1 input
       "2" -> do
         putStrLn $ "Part 2: " ++ solve2 input
       "test" -> do
         putStrLn $ "Testing: " ++ runtests tests input
       _ -> do
         putStrLn $ "ERROR: Unknown part '" ++ (show $ part args) ++ "'!"

