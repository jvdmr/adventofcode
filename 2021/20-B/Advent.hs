module Main where

import Data.List
import Data.List.Split

import Debug.Trace
idtrace x = trace (show x) x
maptrace m = trace (flatten $ map ((++"\n") . map fromBin) m) m

fromBin '0' = ' '
fromBin '1' = '#'

flatten :: [[a]] -> [a]
flatten = foldl (++) []

binToDec :: String -> Int
binToDec [] = 0
binToDec bs = 2 * binToDec (init bs) + read [last bs]

toBin :: Char -> Char
toBin '.' = '0'
toBin '#' = '1'

dots :: Char -> [Char]
dots bchar = [bchar, bchar..]

surroundLine :: Char -> [Char] -> [Char]
surroundLine bchar l = [bchar] ++ l ++ [bchar]

addBorder :: Char -> [[Char]] -> [[Char]]
addBorder bchar image = darkness ++ map (surroundLine bchar) image ++ darkness
  where darkness = [take (2 + length (head image)) $ dots bchar]

parseStuff :: [[String]] -> ([Char], [[Char]])
parseStuff [[algorithm], image] = (map toBin algorithm, maptrace $ addBorder '0' $ map (map toBin) image)

combineLines :: [Char] -> [[Char]] -> [Char]
combineLines algorithm lines | 3 < (length $ head lines) = (applyAlgorithm lines):combineLines algorithm (map tail lines)
                             | otherwise = [applyAlgorithm lines]
                             where applyAlgorithm lines = algorithm !! binToDec (flatten $ map (take 3) lines)

step' :: [Char] -> [[Char]] -> [[Char]]
step' algorithm image | 3 < length image = combineLines algorithm (take 3 image):(step' algorithm $ tail image)
                      | otherwise = [combineLines algorithm image]

step :: ([Char], [[Char]]) -> ([Char], [[Char]])
step (algorithm, image) = (algorithm, maptrace $ step' algorithm $ addBorder bchar $ addBorder bchar image)
  where bchar = head $ head image

count :: [[Char]] -> Int
count = length . filter (== '1') . flatten

main = do
  cnt <- getContents
  print $ count $ snd $ (!!50) $ iterate step $ parseStuff $ splitOn [""] $ lines cnt

