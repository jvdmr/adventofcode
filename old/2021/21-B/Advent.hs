module Main where

-- import Data.List
import Data.List.Split

import Debug.Trace
idtrace x = trace (show x) x

parsePos :: [String] -> ((Int, Int), (Int, Int))
parsePos = pair . map (read. (:[]) . last)
  where pair [a, b] = ((a, 0), (b, 0))

score :: (Int, Int) -> (Int, Int)
score (0, s) = (0, s + 10)
score (p, s) = (p, s + p)

move :: Int -> Int -> Int
move p dr = mod (p + dr) 10

dice :: [Int]
dice = [1, 2, 3]

rolls :: [(Int, Int)]
rolls = map (\l -> (head l, length l)) $ group $ sort [a + b + c | a <- dice, b <- dice, c <- dice]

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (a, b) (c, d) = (a + c, b + d)

multiply :: Int -> (Int, Int) -> (Int, Int)
multiply n (a, b) = (a * n, b * n)

switch :: (a, b) -> (b, a)
switch (a, b) = (b, a)

play :: ((Int, Int), (Int, Int)) -> (Int, Int)
play ((pa, sa), (pb, sb)) | sb >= 21 = (0, 1)
                          | otherwise = switch $ foldl add (0, 0) $ map (\r@(d, _) -> roll ((pb, sb), score (move pa d, sa)) r) rolls
                          where roll ss (d, n) = multiply n $ play ss

main = do
  cnt <- getContents
  print $ uncurry max $ idtrace $ play $ parsePos $ lines cnt

