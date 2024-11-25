module Main where

-- import Data.List
import Data.List.Split

calcArea dimensions = smallest + 2 * (front + side + top)
  where (l:w:h:[]) = map read $ splitOn "x" dimensions
        front = w * h
        side = l * h
        top = w * l
        smallest = head $ sort [front, side, top]

main = do
  cnt <- getContents
  print $ foldl (+) 0 $ map calcArea $ lines cnt

