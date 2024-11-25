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

findSeatInBetween prev (seat:seats) | prev + 2 == seat = prev + 1
                                    | otherwise = findSeatInBetween seat seats

main = do
  cnt <- getContents
  print $ findSeatInBetween 0 $ sort $ map (seatId . parseSeat) $ lines cnt

