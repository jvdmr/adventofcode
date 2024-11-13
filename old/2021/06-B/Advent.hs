module Main where

import Debug.Trace
import Data.List
import Data.List.Split

idtrace x = trace (show x) x

spawnFish (f:fs) = take 6 fs ++ [f + fs !! 6] ++ [fs !! 7] ++ [f]

startSpawn [] = [0, 0, 0, 0, 0, 0, 0, 0, 0]
startSpawn (f:fs) = take f rs ++ [1 + rs !! f] ++ drop (f + 1) rs
  where rs = startSpawn fs

main = do
  cnt <- getContents
--   print $ foldl (+) 0 $ (!!80) $ iterate spawnFish $ startSpawn $ map read $ splitOn "," $ head $ lines cnt
  print $ foldl (+) 0 $ (!!256) $ iterate spawnFish $ startSpawn $ map read $ splitOn "," $ head $ lines cnt
