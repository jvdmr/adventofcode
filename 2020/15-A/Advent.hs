module Main where

import Data.List as L
import Data.List.Split
import Data.Map.Strict as M

calculate 2020 mem cur [] = cur
calculate n mem cur [] | member cur mem = calculate (n+1) (M.insert cur n mem) (n - (mem ! cur)) []
                       | otherwise = calculate (n+1) (M.insert cur n mem) 0 []
calculate n mem cur (x:xs) = calculate (n+1) (M.insert cur n mem) x xs

setup (x:xs) = calculate 1 M.empty x xs

main = do
  cnt <- getContents
  print $ L.map (setup . L.map read . splitOn ",") $ lines cnt

