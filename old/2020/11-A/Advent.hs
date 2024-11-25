module Main where

import Debug.Trace
-- import Data.List
import Text.PrettyPrint hiding (isEmpty)

type Area = [[Char]]

loc :: Area -> (Int, Int) -> Char
loc area (x, y) | within area (x, y) = head $ drop x $ head $ drop y area
                | otherwise = '.'

between a b c = a <= b && b <= c

within area (x, y) = between 0 x (ax-1) && between 0 y (ay-1)
  where (ax, ay) = size area

size :: Area -> (Int, Int)
size area = (length $ head area, length area)

occupiedNum '#' = 1
occupiedNum 'L' = 0
occupiedNum '.' = 0

isOccupied = (==1) . occupiedNum
isEmpty = not . isOccupied

countOccupied :: Area -> Int
countOccupied = foldl (+) 0 . map (foldl (+) 0 . map occupiedNum)

add (a, b) (c, d) = (a + c, b + d)

seatSurroundings area (x, y) = [map (loc area . add (x, y)) $ (-1,-1):(-1,0):(-1,1):(0,-1):(0,1):(1,-1):(1,0):(1,1):[]]

switchSeat '.' = '.'
switchSeat '#' = 'L'
switchSeat 'L' = '#'

evolveSeat area (x, y) | isEmpty seat && countOccupied around == 0 = switchSeat seat
                       | isOccupied seat && countOccupied around >= 4 = switchSeat seat
                       | otherwise = seat
  where seat = loc area (x, y)
        around = seatSurroundings area (x, y)

ignoreMapValue a _ = a

transformArea :: (Area -> (Int, Int) -> b) -> Area -> [[b]]
transformArea f area = map (map (f area) . (\y -> map (\x -> (x,y)) [0..(ax-1)])) [0..(ay-1)]
  where (ax, ay) = size area

evolve :: Area -> Area
evolve = transformArea evolveSeat

stabilize :: Area -> Int
stabilize area | area == newArea = countOccupied area
               | otherwise = stabilize newArea
  where newArea = evolve $ idtrace area

pretty = foldl ($$) (text "") . map text

idtrace x = trace (show $ pretty x) x

main = do
  cnt <- getContents
  print $ stabilize $ lines cnt

