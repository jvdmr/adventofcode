module Main where

-- import Data.List

shift num lst = (drop num lst) ++ (take num lst)

tree ('.':_) = False
tree ('#':_) = True

shiftSlope num slope = map (shift num) slope

countTrees result _ [] = result
countTrees result (left, down) (here:slope) | tree here = countTrees (result + 1) (left, down) $ shiftSlope left $ drop (down - 1) slope
                                            | otherwise = countTrees result (left, down) $ shiftSlope left $ drop (down - 1) slope

combos = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

main = do
  cnt <- getContents
  print $ foldl (*) 1 $ map (\combo -> countTrees 0 combo $ lines cnt) combos

