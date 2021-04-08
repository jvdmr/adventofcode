module Main where

import Data.List

type Coord = (Int, Int)

uniq [] = []
uniq [a] = [a]
uniq (a:b:rst) | a == b = uniq (b:rst)
               | otherwise = a:uniq (b:rst)

nextHouse houses here [] = length $ uniq $ sort (here:houses)
nextHouse houses (here@(x, y)) ('<':dirs) = nextHouse (here:houses) (x - 1, y) dirs
nextHouse houses (here@(x, y)) ('>':dirs) = nextHouse (here:houses) (x + 1, y) dirs
nextHouse houses (here@(x, y)) ('v':dirs) = nextHouse (here:houses) (x, y - 1) dirs
nextHouse houses (here@(x, y)) ('^':dirs) = nextHouse (here:houses) (x, y + 1) dirs

main = do
  cnt <- getContents
  print $ map (nextHouse [] (0, 0)) $ lines cnt

