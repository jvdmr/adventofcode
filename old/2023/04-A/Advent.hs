module Main where

-- import Data.List
import Data.List.Split (splitOn)

import Debug.Trace
idtrace x = trace (show x) x

type Card = ([Int], [Int])

parseCard :: String -> Card
parseCard s = (head nrs, last nrs)
  where nrs = map (map read . filter ((<) 0 . length) . splitOn " ") $ splitOn " | " $ last $ splitOn ": " s

score :: Card -> Int
score (winning, ours) | l > 0 = 2 ^ (l - 1)
                      | otherwise = 0
  where l = length $ filter (flip elem winning) ours

main = do
  cnt <- getContents
  print $ sum $ map (idtrace . score . idtrace . parseCard) $ lines cnt

