module Main where

-- import Data.List
import Data.List.Split

import Debug.Trace
idtrace x = trace (show x) x

uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq [a] = [a]
uniq (a:b:rst) | a == b = uniq (b:rst)
               | otherwise = a:uniq (b:rst)

p [a, b] = (a, b)

toInstr :: [String] -> (Char, Int)
toInstr [a, b] = (last a, read b)

foldBy ('x', n) (x, y) | x > n = (n - (x - n), y)
                       | otherwise = (x, y)
foldBy ('y', n) (x, y) | y > n = (x, n - (y - n))
                       | otherwise = (x, y)

fold :: [(Int, Int)] -> (Char, Int) -> [(Int, Int)]
fold marks instr@('x', n) = uniq $ sort $ map (foldBy instr) $ filter ((/= n) . fst) marks
fold marks instr@('y', n) = uniq $ sort $ map (foldBy instr) $ filter ((/= n) . snd) marks

parseInput :: [String] -> [(Int, Int)]
parseInput input = fold marks $ idtrace $ head $ map (toInstr . splitOn "=") $ tail $ dropWhile (/= "") input
  where marks = uniq $ sort $ map (p . map read . splitOn ",") $ takeWhile (/= "") input

main = do
  cnt <- getContents
  print $ parseInput $ lines cnt
  print $ length $ parseInput $ lines cnt

