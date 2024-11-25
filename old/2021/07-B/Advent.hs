module Main where

-- import Data.List
import Data.List.Split

summate :: (Integral a) => a -> a
summate n = n * (n+1) `div` 2

fuel :: (Show a, Integral a) => [a] -> a -> a
fuel ns m = foldl (+) 0 $ map (summate . abs . (m -)) ns

allfuel :: (Show a, Integral a) => [a] -> [a]
allfuel ns = map (fuel ns) [1..m]
  where m = last $ sort ns

main = do
  cnt <- getContents
  print $ head $ sort $ allfuel $ map read $ splitOn "," $ head $ lines cnt

