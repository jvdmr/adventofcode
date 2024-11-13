module Main where

import Data.List hiding (insert)

import Debug.Trace
idtrace x = trace (show x) x

combineBuckets :: Num a => [a] -> [[a]]
combineBuckets [] = []
combineBuckets (bucket:others) = [[bucket]] ++ map (bucket:) otherCombinations ++ otherCombinations
  where otherCombinations = combineBuckets others

fillBuckets limit buckets = limit == foldl (+) 0 buckets

minBuckets buckets = filter ((== n) . length) buckets
  where n = head $ sort $ map length buckets

-- eggnog = 25
eggnog = 150

main = do
  cnt <- getContents
  print $ length $ minBuckets $ filter (fillBuckets eggnog) $ combineBuckets $ map read $ lines cnt

