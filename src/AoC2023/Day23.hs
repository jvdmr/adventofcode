{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts, TypeFamilies #-}
module AoC2023.Day23
  ( part1
  , part2
  ) where

import Data.List
import qualified Data.Map as M

import AoC (Solver, bfs)
import AoC.Grid

type GCoord = Coord Int

data Trail = Path
           | Forest
           | Slope Direction
           deriving (Eq)

instance Show Trail where
  show Path = "."
  show Forest = "#"
  show (Slope U) = "^"
  show (Slope R) = ">"
  show (Slope D) = "v"
  show (Slope L) = "<"

readTrails :: Char -> Trail
readTrails '.' = Path
readTrails '#' = Forest
readTrails '^' = Slope U
readTrails '>' = Slope R
readTrails 'v' = Slope D
readTrails '<' = Slope L

start :: Grid Trail -> GCoord
start (Grid g) = (x, 0)
  where x = length $ takeWhile (/= Path) $ head g

end :: Grid Trail -> GCoord
end (Grid g) = (x, y)
  where x = length $ takeWhile (/= Path) $ last g
        y = length g - 1

nodes :: Grid Trail -> [GCoord]
nodes = coords

path :: Grid Trail -> GCoord -> Bool
path grid xy = inGrid grid xy && Forest /= grid ! xy

dir :: Trail -> Direction
dir (Slope d) = d

neighbors :: Grid Trail -> GCoord -> [GCoord]
neighbors grid xy | xy == end grid = [xy]
                  | Forest == c = []
                  | Path == c = filter (path grid) [go d xy | d <- [D, R, U, L]]
                  | otherwise = [go (dir c) xy]
                  where c = grid ! xy

follow :: Grid Trail -> [GCoord] -> GCoord -> [GCoord]
follow grid trail@(from:_) xy | from == xy = init trail
                              | l == 0 = []
                              | l > 1 = init $ xy:trail
                              | otherwise = follow grid (xy:trail) $ head n
                              where n = filter (/= from) $ neighbors grid xy
                                    l = length n

trails :: Grid Trail -> GCoord -> [[GCoord]]
trails grid xy@(19, 19) = filter (/= []) $ map (follow grid [xy]) $ neighbors grid xy
trails grid xy = filter (/= []) $ map (follow grid [xy]) $ neighbors grid xy

type Graph = M.Map GCoord [[GCoord]]

lastOrEmpty :: [a] -> [a]
lastOrEmpty [] = []
lastOrEmpty as = [last as]

fromGrid :: Grid Trail -> Graph
fromGrid grid = M.fromList $ zip nodes $ map nb nodes
  where nodes = coords grid
        nb = reverse . sortOn length . trails grid

hike :: GCoord -> GCoord -> Graph -> Int
hike s e g = pred $ maximum $ map (length . concat) $ filter ((== e) . head . head) $ bfs nb prune [] [[[s]]]
  where nb trail@((xy:_):_) = map (:trail) $ filter (flip notElem (map head trail) . head) $ g M.! xy
        prune = id

part1 :: Solver
part1 = show . h . Grid . map (map readTrails) . lines
  where h grid = hike (start grid) (end grid) $ fromGrid grid

readTrails' :: Char -> Trail
readTrails' '#' = Forest
readTrails' _ = Path

part2 :: Solver
part2 = show . h . Grid . map (map readTrails') . lines
  where h grid = hike (start grid) (end grid) $ fromGrid grid

