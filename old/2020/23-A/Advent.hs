module Main where

import Data.List

crabcups :: Int -> Int -> [Int] -> [Int]
crabcups min max (a:as) = as' ++ [a]
  where as' = before ++ taken ++ after
        taken = take 3 as
        tas = drop 3 as
        before = (takeWhile (/= d) tas) ++ [d]
        after = tail $ dropWhile (/= d) tas
        d = findDest (a - 1)
        findDest n | n < min = findDest max
                   | not $ elem n tas = findDest (n - 1)
                   | otherwise = n

repeatF :: Int -> (Int -> Int -> [Int] -> [Int]) -> [Int] -> [Int]
repeatF n f ls = foldl f' ls [1..n]
  where f' a _ = f minA maxA a
        maxA = head $ reverse $ sortedLs
        minA = head $ sortedLs
        sortedLs = sort ls

rotateWhile f (a:as) | f a = rotateWhile f (as ++ [a])
                     | otherwise = a:as

main = do
  cnt <- getContents
  print $ foldl (++) "" $ map show $ tail $ rotateWhile (/= 1) $ repeatF 100 crabcups $ map (read . (:[])) $ head $ lines cnt

