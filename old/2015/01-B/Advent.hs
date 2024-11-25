module Main where

-- import Data.List

findBasement p (-1) _ = p
findBasement _ _ [] = 0
findBasement p f ('(':rst) = findBasement (p + 1) (f + 1) rst
findBasement p f (')':rst) = findBasement (p + 1) (f - 1) rst

main = do
  cnt <- getContents
  print $ map (findBasement 0 0) $ lines cnt

