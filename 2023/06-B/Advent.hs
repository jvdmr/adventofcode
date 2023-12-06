module Main where

import Data.List
import Data.List.Split (splitOn)

import Debug.Trace
idtrace x = trace (show x) x

type Time = Int
type Distance = Int
type Race = (Time, Distance)

-- wins :: Race -> [Time]
-- wins (time, distance) = filter (win . traveled) [0..time]
--   where win d = d > distance
--         traveled t = (time - t) * t

wins :: Race -> Int
wins (time, distance) = most - least + 1
  where win d = d > distance
        traveled t = (time - t) * t
        least = head $ dropWhile (not . win . traveled) [0..time]
        most = head $ dropWhile (not . win . traveled) [time, time - 1 .. 0]

cleanInput :: String -> Int
cleanInput = read . foldl (++) "" . tail . splitOn " "

compileRace :: [Int] -> Race
compileRace [time, distance] = (time, distance)

main = do
  cnt <- getContents
--   print $ length $ wins $ compileRace $ map cleanInput $ lines cnt
  print $ wins $ compileRace $ map cleanInput $ lines cnt

