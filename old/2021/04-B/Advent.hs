module Main where

import Debug.Trace

import Data.List
import Data.List.Split

idtrace x = trace (show x) x

type BNum = (Bool, Int)
type Board = ([[BNum]], Int)

board :: [String] -> Board
board ss = (map (map ((,) False . read) . filter (/= "") . splitOn " ") ss, -1)

winboard :: Board -> Bool
winboard b = foldl (||) False $ map winrow $ (fst b) ++ cols (fst b)
  where cols b = map (\f -> f b) $ map (\n -> map (head . drop n)) [0..4]
        winrow r = [] == filter (not . fst) r

findWin :: [Board] -> [Board]
findWin boards = filter (not . winboard) boards

markN :: Int -> [Board] -> [Board]
markN n boards = map ((\b -> (b, n)) . map (map marker) . fst) boards
  where marker (m, i) | i == n = (True, i)
                      | otherwise = (m, i)

callNumber :: [Board] -> Int -> [Board]
callNumber [win] n | winboard win = [win]
                   | otherwise = markN n $ findWin [win]
callNumber boards n = markN n $ findWin boards

bingo (numberline:_:boardlines) = foldl callNumber boards numbers
  where boards = map board $ splitOn [""] boardlines
        numbers = map read $ splitOn "," numberline

checksum :: Board -> Int
checksum (b, n) = n * (foldl (+) 0 $ map snd $ filter (not . fst) $ concat b)

main = do
  cnt <- getContents
  print $ checksum $ idtrace $ head $ bingo $ lines cnt

