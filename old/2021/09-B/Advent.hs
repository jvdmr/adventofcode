module Main where

-- import Data.List

import Control.Concurrent
import Debug.Trace
idtrace x = trace (show x) x
basintrace :: [[Int]] -> [[(Int, Int)]] -> [(Int, Int)] -> [(Int, Int)]
basintrace iss results result = trace (foldl (++) "" $ map (\x -> (++"\n") $ foldl (++) "" $ map (lookup x) [0..maxY]) [0..maxX]) result
  where lookup x y | elem (x, y) $ concat (result:results) = " "
                   | otherwise = showH $ (iss !! x) !! y
        maxX = (length iss) - 1
        maxY = (length $ head iss) - 1
        showH 9 = "#"
        showH x = show x

uniq [] = []
uniq [a] = [a]
uniq (a:b:rst) | a == b = uniq (b:rst)
               | otherwise = a:uniq (b:rst)

findLow :: [[Int]] -> [(Int, Int)]
findLow iss = filter isLow $ concat $ map (\x -> map ((,) x) [0..maxY]) [0..maxX]
  where maxX = (length iss) - 1
        maxY = (length $ head iss) - 1
        lookup x y | x < 0 || y < 0 || x > maxX || y > maxY = 999
                   | otherwise = (iss !! x) !! y
        isLow (x, y) = val < up && val < down && val < left && val < right
          where val = lookup x y
                up = lookup (x - 1) y
                down = lookup (x + 1) y
                left = lookup x (y - 1)
                right = lookup x (y + 1)

basin :: [[Int]] -> [[(Int, Int)]] -> (Int, Int) -> [[(Int, Int)]]
basin iss results (x, y) = findBasin [] [] [(x, y)]
--   where findBasin result _ [] = (basintrace iss results result):results
--         findBasin result seen ((x, y):rest) | ((iss !! x) !! y) == 9 = findBasin (basintrace iss results result) ((x, y):seen) rest
--                                             | otherwise = (findBasin (basintrace iss results $ (x, y):result) ((x, y):seen) $ filter (not . flip elem seen) $ filter valid ((x - 1, y):(x + 1, y):(x, y - 1):(x, y + 1):rest))
  where findBasin result _ [] = result:results
        findBasin result seen ((x, y):rest) = findBasin ((x, y):result) ((x, y):seen) $ uniq $ sort $ filter (valid seen) ((x - 1, y):(x + 1, y):(x, y - 1):(x, y + 1):rest)
        maxX = (length iss)
        maxY = (length $ head iss)
        valid seen (x, y) = x > -1 && y > -1 && x < maxX && y < maxY && ((iss !! x) !! y) /= 9 && (not $ elem (x, y) seen)

findBasinSizes :: [[Int]] -> [[(Int, Int)]]
findBasinSizes iss = foldl (basin iss) [] $ reverse $ findLow iss

compareLength a b = compare (length b) (length a)

main = do
  cnt <- getContents
  print $ foldl (*) 1 $ idtrace $ map length $ map idtrace $ take 3 $ sortBy compareLength $ findBasinSizes $ map (map (read . (:[]))) $ lines cnt

