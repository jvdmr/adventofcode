module Main where

import Data.List
import Data.Char

import Debug.Trace
idtrace x = trace (show x) x

-- uniq is better than nub on sorted lists
uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq [a] = [a]
uniq (a:b:rst) | a == b = uniq (b:rst)
               | otherwise = a:uniq (b:rst)

flatten :: [[a]] -> [a]
flatten = foldl (++) []

type Coord = (Int, Int)

add :: Coord -> Coord -> Coord
add (a, b) (c, d) = (a + c, b + d)

data Grid a = Grid [[a]]
  deriving (Eq)

instance (Show a, Eq a) => Show (Grid a) where
  show (Grid g) = flatten $ map ((++ "\n") . show) g

(!) :: Grid a -> Coord -> a
(!) (Grid g) (x, y) = (g !! y) !! x

mapG :: (a -> b) -> Grid a -> Grid b
mapG f (Grid g) = Grid $ map (map f) g

coords :: Grid a -> [Coord]
coords (Grid g) = [(x, y) | x <- [0..mx], y <- [0..my]] 
  where my = length g - 1
        mx = length (head g) - 1

surround :: Coord -> [Coord]
surround (x, y) = [(xn, yn) | xn <- map (+x) [-1, 0, 1], yn <- map (+y) [-1, 0, 1], xn /= x || yn /= y]

surroundAll :: [Coord] -> [Coord]
surroundAll cs = filter (flip notElem cs) $ uniq $ sort $ flatten $ map surround cs

--

leadAndTrail :: a -> [a] -> [a]
leadAndTrail e ls = [e] ++ ls ++ [e]

pad :: a -> [[a]] -> [[a]]
pad e g = map (leadAndTrail e) $ leadAndTrail extraLine g
  where extraLine = take (length $ head g) $ repeat e

dotsAreSpaces :: Char -> Char
dotsAreSpaces '.' = ' '
dotsAreSpaces c = c

--

isFirstDigit :: Grid Char -> Coord -> Bool
isFirstDigit g c = isDigit v && not (isDigit v')
  where v = g ! c
        v' = g ! (add c (-1, 0))

firstDigit :: Grid Char -> Coord -> Coord
firstDigit g c | isFirstDigit g c = c
               | otherwise = firstDigit g c'
  where c' = add c (-1, 0)

number :: Grid Char -> Coord -> Int
number g c = read $ takeWhile isDigit $ map ((!) g . add c . flip (,) 0) $ [0..]

isGearSymbol :: Char -> Bool
isGearSymbol '*' = True
isGearSymbol _ = False

gearSymbols :: Grid Char -> [Coord]
gearSymbols g = filter (isGearSymbol . (!) g) xys
  where xys = coords g

type Gear = [Int]

isGear :: Gear -> Bool
isGear g = 2 == length g

gear :: Grid Char -> Coord -> Gear
gear g c = map (number g) $ uniq $ sort $ map (firstDigit g) $ filter (isDigit . (!) g) $ surround c

gears :: Grid Char -> [Gear]
gears g = filter isGear $ map (gear g) $ gearSymbols g

ratio :: Gear -> Int
ratio [a, b] = a * b

main = do
  cnt <- getContents
  print $ sum $ map ratio $ gears $ Grid $ pad ' ' $ lines $ map dotsAreSpaces cnt

