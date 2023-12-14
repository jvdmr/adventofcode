module Main where

import Data.List
import Data.List.Split

import Debug.Trace
idtrace x = trace (show x) x

flatten :: [[a]] -> [a]
flatten = foldl (++) []

rotate :: [[a]] -> [[a]]
rotate = transpose . map reverse

east :: [String] -> [String]
east = map $ flatten . map sort . split (keepDelimsL $ oneOf "#")

north :: [String] -> [String]
north = rotate . east . rotate . rotate . rotate

loadNorth :: [String] -> Int
loadNorth = sum . zipWith (*) [1..] . reverse . map (length . filter (== 'O'))

main = do
  cnt <- getContents
  print $ loadNorth $ north $ lines cnt

