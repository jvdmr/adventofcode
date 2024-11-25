module Main where

-- import Data.List
import Data.List.Split

import Debug.Trace
idtrace x = trace (show x) x
ftrace f x = trace (f x) x

infl x = x:infl x

type Pixel = Bool

type Screen = [[Pixel]]

screen :: Int -> Int -> Screen
screen x y = take y $ infl fs
  where fs = take x $ infl False

showPixel :: Pixel -> Char
showPixel True = '#'
showPixel False = ' '

showScreen :: Screen -> String
showScreen s = concat $ map ((++ "\n") . map showPixel) s

rect :: Int -> Int -> Screen -> Screen
rect a b s = (map (((take a [True, True ..]) ++) . drop a) $ take b s) ++ (drop b s)

shift :: Int -> [Pixel] -> [Pixel]
shift n r = (drop d r) ++ (take d r)
  where l = length r
        d = l - (mod n l)

rotate :: String -> Int -> Int -> Screen -> Screen
rotate "row" a b s = (take a s) ++ [(shift b $ head $ drop a s)] ++ (drop (a + 1) s)
rotate "column" a b s = transpose $ rotate "row" a b $ transpose s

parseInstruction :: [String] -> Screen -> Screen
parseInstruction ("rect":details) = rect a b
  where [a, b] = map read $ splitOn "x" $ concat details
parseInstruction ("rotate":rc:details) = rotate rc a b
  where [a, b] = map read $ splitOn "by" $ head $ tail $ splitOn "=" $ concat details

execute :: Screen -> String -> Screen
execute s ins = parseInstruction (splitOn " " ins) s

main = do
  cnt <- getContents
  print $ length $ filter id $ concat $ ftrace showScreen $ foldl execute (screen 50 6) $ lines cnt

