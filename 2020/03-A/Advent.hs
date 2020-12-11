module Main where

import Data.List

shift3 (a:b:c:rst) = rst ++ [a, b, c]

tree ('.':_) = False
tree ('#':_) = True

shiftSlope slope = map shift3 slope

countTrees result [] = result
countTrees result (here:slope) | tree here = countTrees (result + 1) (shiftSlope slope)
                               | otherwise = countTrees result (shiftSlope slope)

main = do
  cnt <- getContents
  print $ countTrees 0 $ lines cnt

