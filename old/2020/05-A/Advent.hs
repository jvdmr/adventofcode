module Main where

-- import Data.List
import Data.Char

row 'F' = 0
row 'B' = 1
col 'L' = 0
col 'R' = 1

parseBin toBin = foldr (\x y -> toBin x + 2 * y) 0 . reverse

parseSeat code = (parseBin row $ take 7 code, parseBin col $ drop 7 code)

seatId (row, col) = row * 8 + col

main = do
  cnt <- getContents
  print $ sort $ map (seatId . parseSeat) $ lines cnt

