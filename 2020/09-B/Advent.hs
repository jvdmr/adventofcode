module Main where

import Data.List

wrongNumber = 1124361034
-- wrongNumber = 127

sumSet preamble rest | (foldl (+) 0 preamble) == wrongNumber = preamble
                     | (foldl (+) 0 preamble) > wrongNumber = sumSet (tail preamble) rest
                     | otherwise = sumSet (preamble ++ (take 1 rest)) $ tail rest

findSet numbers = sumSet (take 2 numbers) $ drop 2 numbers

checkSum numbers = (head sn) + (last sn)
  where sn = sort numbers

main = do
  cnt <- getContents
  print $ findSet $ map read $ lines cnt
  print $ checkSum $ findSet $ map read $ lines cnt

