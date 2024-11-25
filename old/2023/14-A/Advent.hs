module Main where

-- import Data.List
import Data.List.Split

import Debug.Trace
idtrace x = trace (show x) x

rotate :: [[a]] -> [[a]]
rotate = transpose . map reverse

east :: [String] -> [String]
east = map $ concat . map sort . split (keepDelimsL $ oneOf "#")

north :: [String] -> [String]
north = rotate . east . rotate . rotate . rotate

loadNorth :: [String] -> Int
loadNorth = sum . zipWith (*) [1..] . reverse . map (length . filter (== 'O'))

main = do
  cnt <- getContents
  print $ loadNorth $ north $ lines cnt

