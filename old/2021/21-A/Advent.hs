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
dice = [(mod x 100) + 1 | x <- [0..]]

roll :: Int -> Int
roll d = foldl (+) 0 $ take 3 $ drop d dice

play :: Int -> ((Int, Int), (Int, Int)) -> Int
play d ((pa, sa), (pb, sb)) | sb >= 1000 = sa * d
                            | otherwise = play (d + 3) ((pb, sb), score (move pa $ roll d, sa))

main = do
  cnt <- getContents
  print $ play 0 $ parsePos $ lines cnt

