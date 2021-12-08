module Main where

import Data.List
import Data.List.Split

median :: [Int] -> Int
median ns = (sort ns) !! i
  where i = length ns `div` 2

fuel :: [Int] -> Int
fuel ns = foldl (+) 0 $ map (abs . (m -)) ns
  where m = median ns

main = do
  cnt <- getContents
  print $ fuel $ map read $ splitOn "," $ head $ lines cnt

