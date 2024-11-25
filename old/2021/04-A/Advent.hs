module Main where

import Debug.Trace

-- import Data.List
import Data.List.Split

idtrace x = trace (show x) x

type Board = ([[Int]], Int)

board :: [String] -> Board
board ss = (map (map read . filter (/= "") . splitOn " ") ss, -1)

findWin :: [Board] -> [Board]
findWin boards = returnOnlyWin $ filter winboard boards
  where cols b = map (\f -> f b) $ map (\n -> map (head . drop n)) [0..4]
        winrow r = [] == filter (>= 0) r
        winboard b = foldl (||) False $ map winrow $ (fst b) ++ cols (fst b)
        returnOnlyWin [] = boards
        returnOnlyWin b = b

markN :: Int -> [Board] -> [Board]
markN n boards = map ((\b -> (b, n)) . map (map marker) . fst) boards
  where marker i | i == n = -i
                 | otherwise = i

callNumber :: [Board] -> Int -> [Board]
callNumber [win] _ = [win]
callNumber boards n = findWin $ markN n boards

bingo (numberline:_:boardlines) = foldl callNumber boards numbers
  where boards = map board $ splitOn [""] boardlines
        numbers = map read $ splitOn "," numberline

checksum :: Board -> Int
checksum (b, n) = n * (foldl (+) 0 $ filter (> 0) $ concat b)

main = do
  cnt <- getContents
  print $ checksum $ head $ bingo $ lines cnt

