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

digitsAndBorder :: Grid Char -> Coord -> (Int, String)
digitsAndBorder g c = (read $ map ((!) g) digits, map ((!) g) $ surroundAll digits)
  where digits = takeWhile (isDigit . (!) g) $ map (add c . flip (,) 0) $ [0..]

findNumbers :: Grid Char -> [(Int, String)]
findNumbers g = map (digitsAndBorder g) $ filter (isFirstDigit g) xys
  where xys = coords g

isValidSymbol :: Char -> Bool
isValidSymbol c = not (isDigit c) && not (isSpace c)

findPartNumbers :: Grid Char -> [Int]
findPartNumbers g = map fst $ filter ((<) 0 . length . filter isValidSymbol . snd) $ findNumbers g

main = do
  cnt <- getContents
  print $ Grid $ pad ' ' $ lines $ map dotsAreSpaces cnt
  print $ sum $ findPartNumbers $ Grid $ pad ' ' $ lines $ map dotsAreSpaces cnt

