module Main where

-- import Data.List
import Data.List.Split

import Debug.Trace
idtrace x = trace (show x) x
maptrace m = trace (concat $ map ((++"\n") . map fromBin) m) m

fromBin '0' = ' '
fromBin '1' = '#'

binToDec :: String -> Int
binToDec [] = 0
binToDec bs = 2 * binToDec (init bs) + read [last bs]

toBin :: Char -> Char
toBin '.' = '0'
toBin '#' = '1'

dots :: [Char]
dots = ['0','0'..]

surroundLine :: [Char] -> [Char]
surroundLine l = ['0'] ++ l ++ ['0']

addBorder :: [[Char]] -> [[Char]]
addBorder image = darkness ++ map surroundLine image ++ darkness
  where darkness = [take (2 + length (head image)) dots]

parseStuff :: [[String]] -> ([Char], [[Char]])
parseStuff [[algorithm], image] = (map toBin algorithm, maptrace $ (!!10) $ iterate addBorder $ map (map toBin) image)

combineLines :: [Char] -> [[Char]] -> [Char]
combineLines algorithm lines | 3 < (length $ head lines) = (applyAlgorithm lines):combineLines algorithm (map tail lines)
                             | otherwise = [applyAlgorithm lines]
                             where applyAlgorithm lines = algorithm !! binToDec (concat $ map (take 3) lines)

step' :: [Char] -> [[Char]] -> [[Char]]
step' algorithm image | 3 < length image = combineLines algorithm (take 3 image):(step' algorithm $ tail image)
                      | otherwise = [combineLines algorithm image]

step :: ([Char], [[Char]]) -> ([Char], [[Char]])
step (algorithm, image) = (algorithm, maptrace $ step' algorithm image)

count :: [[Char]] -> Int
count = length . filter (== '1') . concat

main = do
  cnt <- getContents
  print $ count $ snd $ (!!2) $ iterate step $ parseStuff $ splitOn [""] $ lines cnt

