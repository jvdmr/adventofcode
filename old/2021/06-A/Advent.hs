module Main where

import Data.List
import Data.List.Split

spawnFish :: Int -> [Int] -> [Int]
spawnFish 0 fish = fish
spawnFish days fish = spawnFish (days - 1) $ ((map checkSpawn fish) ++ (map (\_ -> 8) $ filter (== 0) fish))
  where checkSpawn 0 = 6
        checkSpawn n = n - 1

main = do
  cnt <- getContents
  print $ length $ spawnFish 80 $ map read $ splitOn "," $ head $ lines cnt

