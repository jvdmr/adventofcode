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

possibleCubeCount :: Set -> CubeCount -> Bool
possibleCubeCount b (c, n) = (b ! c) >= n

possibleSet :: Set -> Set -> Bool
possibleSet b s = all (possibleCubeCount b) $ M.toList s

possibleGame :: Set -> Game -> Bool
possibleGame b (_, ss) = all (possibleSet b) ss

bag :: Set
bag = M.fromList [(Red, 12), (Green, 13), (Blue, 14)]

main = do
  cnt <- getContents
  print $ sum $ map fst $ filter (possibleGame bag) $ map parseGame $ lines cnt

