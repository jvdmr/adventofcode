module Main where

-- import Data.List

combineLines :: String -> [String] -> String
combineLines cur [] = cur
combineLines cur ("":l:rst) = cur ++ (combineLines l rst)
combineLines cur (l:rst) = combineLines (filter (flip elem l) cur) rst

main = do
  cnt <- getContents
  print $ length $ (\(l:rst) -> combineLines l rst) $ lines cnt

