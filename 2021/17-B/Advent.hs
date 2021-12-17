module Main where

import Data.List
import Data.List.Split

import Debug.Trace
idtrace x = trace (show x) x

flatten :: [[a]] -> [a]
flatten = foldl (++) []

parseArea :: String -> (Int, Int, Int, Int)
parseArea s = (x1, x2, y1, y2)
  where [x1, x2, y1, y2] = map read $ flatten $ map (splitOn ".." . last . splitOn "=") $ splitOn ", " $ last $ splitOn ": " s

to0 :: Int -> Int
to0 0 = 0
to0 x | x > 0 = x - 1
      | otherwise = x + 1

next :: (Int, Int) -> (Int, Int)
next (x, y) = (to0 x, y - 1)

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (a, b) (c, d) = (a + c, b + d)

points :: (Int, Int) -> [(Int, Int)]
points p = zipWith add ((0, 0):points p) $ iterate next p

solveEq :: (Int, Int, Int, Int) -> (Int, Int) -> Maybe [(Int, Int)]
solveEq (xa, xb, ya, yb) v = just $ takeWhile valid $ points v
  where valid (x, y) = x <= xb && y >= ya
        just line | xa <= (fst $ last line) && yb >= (snd $ last line) = Just line
                  | otherwise = Nothing

summate 0 = 0
summate x = x + summate (x - 1)

findStartX x1 x2 = takeWhile valid $ dropWhile invalid [0..]
  where valid x = x1 <= summate x && x2 >= x
        invalid = not . valid

findStartY y1 y2 = [y1..abs(y1)]

trajectories :: (Int, Int, Int, Int) -> Int
trajectories area@(x1, x2, y1, y2) = length $ filter valid $ map (solveEq area) startPoints
  where startPoints = [(x, y) | x <- findStartX x1 x2, y <- findStartY y1 y2]
        valid Nothing = False
        valid _ = True

main = do
  cnt <- getContents
  print $ map (trajectories . parseArea) $ lines cnt

