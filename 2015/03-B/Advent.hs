module Main where

import Data.List

type Coord = (Int, Int)

uniq [] = []
uniq [a] = [a]
uniq (a:b:rst) | a == b = uniq (b:rst)
               | otherwise = a:uniq (b:rst)

nextHouse houses hereA hereB [] = length $ uniq $ sort (hereA:hereB:houses)
nextHouse houses (hereA@(x, y)) hereB ('<':dirs) = nextHouse (hereA:houses) hereB (x - 1, y) dirs
nextHouse houses (hereA@(x, y)) hereB ('>':dirs) = nextHouse (hereA:houses) hereB (x + 1, y) dirs
nextHouse houses (hereA@(x, y)) hereB ('v':dirs) = nextHouse (hereA:houses) hereB (x, y - 1) dirs
nextHouse houses (hereA@(x, y)) hereB ('^':dirs) = nextHouse (hereA:houses) hereB (x, y + 1) dirs

main = do
  cnt <- getContents
  print $ map (nextHouse [] (0, 0) (0, 0)) $ lines cnt

