{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Vdmr.Generic (Year)

import AoC2023

import Data.List.Split (splitOn)
import Data.Map (Map, fromList, (!))
import System.Console.CmdArgs.Implicit (Data, Typeable, args, cmdArgs, def, help, opt, (&=))

data Arguments = Arguments { yeardaystr :: String, part :: Int }
  deriving (Show, Data, Typeable)

years :: Map Int Year
years = fromList [ (2023, AoC2023.days) ]

main :: IO ()
main = do
  let arguments =
        Arguments
          { yeardaystr = def &= help "Which year/day to process"
          , part = def &= help "Which part to run. If not specified, both parts are run" &= opt (0::Int)
          }
  args <- cmdArgs arguments
  let theDate = yeardaystr args
      [theYear, theDay] = map read $ splitOn "/Day" theDate
      (solve1, solve2) = years ! theYear ! theDay
  input <- getContents
  case part args of
       0 -> do
         print $ solve1 input
         print $ solve2 input
       1 -> do
         print $ solve1 input
       2 -> do
         print $ solve2 input

