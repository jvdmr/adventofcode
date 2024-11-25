module Main where

-- import Data.List
import Data.List.Split

guess :: String -> Int
guess s | length s == 2 = 1
        | length s == 3 = 7
        | length s == 4 = 4
        | length s == 5 = 5 -- or 2, 3
        | length s == 6 = 0 -- or 6, 9
        | length s == 7 = 8

process :: String -> [Int]
process = map guess . last . map (splitOn " ") . splitOn " | "

main = do
  cnt <- getContents
  print $ length $ filter (flip elem [1, 4, 7, 8]) $ foldl (++) [] $ map process $ lines cnt

