module Main where

-- import Data.List

import Debug.Trace
idtrace x = trace (show x) x

uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq [a] = [a]
uniq (a:b:rst) | a == b = uniq (b:rst)
               | otherwise = a:uniq (b:rst)

data Grid a = Grid [[a]]
  deriving (Eq)

instance (Show a, Eq a) => Show (Grid a) where
  show (Grid g) = concat $ map ((++ "\n") . show) g

type Coord = (Int, Int)

up :: Coord -> Coord
up (x, y) = (x, y - 1)
down :: Coord -> Coord
down (x, y) = (x, y + 1)
left :: Coord -> Coord
left (x, y) = (x - 1, y)
right :: Coord -> Coord
right (x, y) = (x + 1, y)

(!) :: Grid a -> Coord -> a
(!) (Grid g) (x, y) = (g !! x) !! y

mapG :: (a -> b) -> Grid a -> Grid b
mapG f (Grid g) = Grid $ map (map f) g

setG :: Coord -> a -> Grid a -> Grid a
setG (x, y) v (Grid g) = Grid $ take x g ++ [(take y (g !! x) ++ [v] ++ drop (y + 1) (g !! x))] ++ drop (x + 1) g

swp :: [Coord] -> [Coord] -> Grid Int -> Grid Int -> Grid Int
swp visited (cur@(cx, cy):tbc) distances grid@(Grid g) | cx == mx && cy == my = distances
                                                       | otherwise = swp (cur:visited) closest updatedDistances grid
  where curDist = distances ! cur
        mx = length g - 1
        my = length (head g) - 1
        valid (x, y) = 0 <= x && x <= mx && 0 <= y && y <= my
        seen = not . flip elem visited
        neighbours = filter seen $ filter valid [up cur, down cur, left cur, right cur]
        updatePos g pos | distances ! pos <= curDist + grid ! pos = g
                        | otherwise = setG pos (curDist + grid ! pos) g
        updatedDistances = foldl updatePos distances neighbours
        closest = uniq $ map snd $ sort $ map (\n -> (updatedDistances ! n, n)) $ (tbc ++ neighbours)

shortestWeightedPath :: Grid Int -> Int
shortestWeightedPath grid@(Grid g) = (swp [] [(0, 0)] (setG (0, 0) 0 $ mapG (\_ -> 99999999999) grid) grid) ! (mx, my)
  where mx = length g - 1
        my = length (head g) - 1

raiseRisk :: Int -> Int
raiseRisk 9 = 1
raiseRisk a = a + 1

fullMap :: Grid Int -> Grid Int
fullMap (Grid g) = Grid $ foldl (++) [] $ take 5 $ iterate raiseGridLine $ gridLine g
  where raiseLine = foldl (++) [] . take 5 . iterate (map raiseRisk)
        gridLine g = map raiseLine g
        raiseGridLine = map (map raiseRisk)

main = do
  cnt <- getContents
  print $ shortestWeightedPath $ fullMap $ mapG (read . (:[])) $ Grid $ lines cnt

