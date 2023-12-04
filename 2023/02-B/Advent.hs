module Main where

import Data.List
import Data.List.Split (splitOn)
import Data.Map ((!))
import qualified Data.Map as M

import Debug.Trace
idtrace x = trace (show x) x

data Color = Red | Green | Blue
  deriving (Show, Eq, Ord)

readColor :: String -> Color
readColor "red" = Red
readColor "green" = Green
readColor "blue" = Blue

type CubeCount = (Color, Int)
parseCubeCount :: String -> CubeCount
parseCubeCount s = (readColor c, read n)
  where [n, c] = splitOn " " s

type Set = M.Map Color Int
parseSet :: String -> Set
parseSet s = M.fromList $ map parseCubeCount $ splitOn ", " s

type Game = (Int, [Set])
parseGame :: String -> Game
parseGame s = (read id, map parseSet $ splitOn "; " setstr)
  where [idstr, setstr] = splitOn ": " s 
        id = last $ splitOn " " idstr

insertMax :: Set -> Set -> Set
insertMax = M.unionWith max

emptyBag :: Set
emptyBag = M.fromList [(Red, 0), (Green, 0), (Blue, 0)]

leastCubes :: Game -> Set
leastCubes (_, ss) = foldl insertMax emptyBag ss

power :: Set -> Int
power s = foldl (*) 1 $ map snd $ M.toList s

main = do
  cnt <- getContents
  print $ sum $ map (power . leastCubes) $ map parseGame $ lines cnt

