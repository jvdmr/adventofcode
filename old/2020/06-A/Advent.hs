module Main where

-- import Data.List

combineLines :: String -> [String] -> [String]
combineLines cur [] = [cur]
combineLines cur ("":rst) = cur:(combineLines "" rst)
combineLines cur (l:rst) = combineLines (cur ++ l) rst

main = do
  cnt <- getContents
  print $ foldl (+) 0 $ map (length . nub . sort) $ combineLines "" $ lines cnt

