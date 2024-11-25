module Main where

-- import Data.List

findFloor f [] = f
findFloor f ('(':rst) = findFloor (f + 1) rst
findFloor f (')':rst) = findFloor (f - 1) rst

main = do
  cnt <- getContents
  print $ map (findFloor 0) $ lines cnt

