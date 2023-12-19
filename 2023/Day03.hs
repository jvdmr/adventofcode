{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module Day03
  ( part1
  , part2
  ) where

import Data.List
import Data.Char

import Vdmr.Generic
import Vdmr.Grid

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

part1 :: Solver
part1 = show . sum . findPartNumbers . Grid . pad ' ' . lines . map dotsAreSpaces

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

part2 :: Solver
part2 = show . sum . map ratio . gears . Grid . pad ' ' . lines . map dotsAreSpaces

