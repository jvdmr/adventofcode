module Main where

-- import Data.List
import Data.List.Split (splitOn)

import Debug.Trace
idtrace x = trace (show x) x

type Time = Int
type Distance = Int
type Race = (Time, Distance)
type Races = [Race]

wins :: Race -> [Time]
wins (time, distance) = filter (win . traveled) [0..time]
  where win d = d > distance
        traveled t = (time - t) * t

cleanInput :: String -> [Int]
cleanInput = map read . filter (/= "") . tail . splitOn " "

compileRaces :: [[Int]] -> Races
compileRaces [times, distances] = zip times distances

check :: [Int] -> Int
check = foldl (*) 1

main = do
  cnt <- getContents
  print $ check $ map (length . wins) $ compileRaces $ map cleanInput $ lines cnt

