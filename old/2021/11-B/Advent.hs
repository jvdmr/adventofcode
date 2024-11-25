module Main where

-- import Data.List

import Debug.Trace
idtrace x = trace (show x) x

data Grid a = Grid [[a]]
  deriving (Eq)

instance (Show a, Eq a) => Show (Grid a) where
  show (Grid g) = concat $ map ((++ "\n") . show) g

(!) :: Grid a -> (Int, Int) -> a
(!) (Grid g) (x, y) = (g !! x) !! y

mapG :: (a -> b) -> Grid a -> Grid b
mapG f (Grid g) = Grid $ map (map f) g

coords (Grid g) = [(x, y) | x <- [0..mx], y <- [0..my]] 
  where mx = length g - 1
        my = length (head g) - 1

surround :: (Int, Int) -> [(Int, Int)]
surround (x, y) = [(xn, yn) | xn <- map (+x) [-1, 0, 1], yn <- map (+y) [-1, 0, 1], xn /= x || yn /= y]

raise (Grid g) (x, y) = Grid $ take x g ++ [take y linex ++ [1 + (linex !! y)] ++ drop (y + 1) linex] ++ drop (x + 1) g
  where linex = g !! x

propagateFlashes flashes g [] = g
propagateFlashes flashes g neighbours = flash flashes $ foldl raise g neighbours

flash oldflashes grid@(Grid g) = propagateFlashes newflashes fgrid neighbours
  where fgrid = mapG f grid
        flashed = filter ((== 0) . (fgrid !)) $ filter (not . flip elem oldflashes) $ coords fgrid
        newflashes = oldflashes ++ flashed
        neighbours = filter (not . flip elem newflashes) $ filter valid $ concat $ map surround flashed
        valid (x, y) = x >= 0 && y >= 0 && x < lx && y < ly
        lx = length g
        ly = length (head g)
        f x | x > 9 = 0
            | otherwise = x

step :: (Grid Int, Int) -> (Grid Int, Int)
step (g, n) = (flash [] $ mapG (+1) g, n + 1)

flashedGrid :: Grid Int -> Grid Int
flashedGrid g = mapG (\_ -> 0) g

main = do
  cnt <- getContents
  let grid = mapG (read . (:[])) $ Grid $ lines cnt
  print $ head $ dropWhile ((/= flashedGrid grid) . fst) $ iterate step (grid, 0)

