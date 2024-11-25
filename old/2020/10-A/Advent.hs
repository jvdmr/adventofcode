module Main where

-- import Data.List

findDiffs (_:[]) = [3]
findDiffs (m:n:rst) = (n - m):(findDiffs (n:rst))

checkDiffs :: [Integer] -> [Integer]
checkDiffs [] = []
checkDiffs (0:rst) = error "zero?"
checkDiffs (1:rst) = 1:checkDiffs rst
checkDiffs (2:rst) = 2:checkDiffs rst
checkDiffs (3:rst) = 3:checkDiffs rst
checkDiffs (n:rst) = error $ show n

checkSum diffs = (length $ takeWhile (==1) diffs) * (length $ dropWhile (<3) diffs)

main = do
  cnt <- getContents
  print $ checkDiffs $ findDiffs $ (0:) $ sort $ map read $ lines cnt
  print $ checkSum $ sort $ checkDiffs $ findDiffs $ (0:) $ sort $ map read $ lines cnt

