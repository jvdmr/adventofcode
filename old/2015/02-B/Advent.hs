module Main where

-- import Data.List
import Data.List.Split

calcRibbon dimensions = l + l + w + w + l * w * h
  where (l:w:h:[]) = sort $ map read $ splitOn "x" dimensions

main = do
  cnt <- getContents
  print $ foldl (+) 0 $ map calcRibbon $ lines cnt

