{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC2024.Day15
  ( part1
  , part2
  , tests
  ) where

import Prelude hiding (foldl)
import Data.List (foldl', nub)
import Data.List.Split (splitOn)

import AoC (Solver, Tests)
import AoC.Grid (Grid(..), Coord, Direction(..), ungrid, parseDirection, coords, mapG, go, coordsWhere, backwards, combine)
import qualified AoC.Grid as G ((!), insert)
import AoC.Trace

data Floor = Wall
           | Floor
           | Box
           | BigBox Direction
           | Robot
  deriving (Eq, Ord)

instance Show Floor where
--   show Wall = "##"
--   show Box = "[]"
--   show (BigBox R) = " ]"
--   show (BigBox L) = "[ "
  show Wall = "🟦"
  show Box = "📦"
  show (BigBox _) = "📦"
  show Robot = "🤖"
  show _ = "  "

whfloor :: Char -> [Floor]
whfloor '#' = [Wall]
whfloor 'O' = [Box]
whfloor _ = [Floor]

isBox :: Floor -> Bool
isBox Box = True
isBox (BigBox _) = True
isBox _ = False

isBigBox :: Floor -> Bool
isBigBox (BigBox _) = True
isBigBox _ = False

isFloor :: Floor -> Bool
isFloor Floor = True
isFloor _ = False

side :: Floor -> Direction
side (BigBox d) = d

type Robot = Coord Int

data Warehouse = Warehouse { f :: Grid Floor, r :: Robot }
  deriving (Eq)

insert :: Floor -> Coord Int -> Warehouse -> Warehouse
insert x c (Warehouse f r) = Warehouse (G.insert f x c) r

placeRobot :: Warehouse -> Coord Int -> Warehouse
placeRobot (Warehouse f _) r = Warehouse f r

instance Show Warehouse where
  show wh = show $ f $ insert Robot (r wh) wh

robot :: Grid Char -> Robot
robot gc = head $ filter ((==) '@' . (G.!) gc) $ coords gc

warehouse :: (Char -> [Floor]) -> [String] -> Warehouse
warehouse whf ws = Warehouse f (robot gc)
  where gc = Grid ws
        f = Grid $ map concat $ ungrid $ mapG whf gc

parseInput1 :: [[String]] -> (Warehouse, [Direction])
parseInput1 [ws, ds] = (warehouse whfloor ws, map parseDirection $ concat ds)

(!) :: Warehouse -> Coord Int -> Floor
(!) wh c = f wh G.! c

push :: Warehouse -> [Coord Int] -> Direction -> Warehouse
push wh cs d | null boxes = wh
             | d `elem` [L, R] && all (isFloor . (!) wh') cs' = newWH
             | all (isFloor . (!) wh') cs' = newWH
             | otherwise = wh
  where cs' = map (go d) boxes
        wh' = push wh cs' d
        boxes = nub $ concat $ map box cs
        box c | isBigBox (wh ! c) && d `elem` [U, D] = [c, other c]
              | isBox $ wh ! c = [c]
              | otherwise = []
        thisbox c = wh ! c
        other c = (go $ backwards $ side $ thisbox c) c
        newWH = foldl' (flip $ uncurry insert) wh' $ concat [[(Floor, c), (thisbox c, go d c)] | c <- boxes]

move :: Warehouse -> Direction -> Warehouse
move wh d | wh ! r' == Wall = wh
          | isBox (wh' ! r') = wh
          | otherwise = wh'
  where r' = go d $ r wh
        wh' = placeRobot (push wh [r'] d) r'

boxGps :: Coord Int -> Int
boxGps (x, y) = x + 100 * y

isGpsBox :: Floor -> Bool
isGpsBox Box = True
isGpsBox (BigBox L) = True
isGpsBox _ = False

gps :: Warehouse -> Int
gps wh = sum $ map boxGps $ coordsWhere isGpsBox $ f wh

tests :: Tests
tests = [show . length . lines]

part1 :: Solver
part1 = show . gps . idtrace . uncurry (foldl' move) . idtrace . parseInput1 . splitOn [""] . lines

whfloor' :: Char -> [Floor]
whfloor' '#' = [Wall, Wall]
whfloor' 'O' = [BigBox L, BigBox R]
whfloor' _ = [Floor, Floor]

parseInput2 :: [[String]] -> (Warehouse, [Direction])
parseInput2 [ws, ds] = (warehouse whfloor' ws, map parseDirection $ concat ds)

fixRobot :: (Warehouse, a) -> (Warehouse, a)
fixRobot (wh, x) = flip (,) x $ placeRobot wh $ combine (*) (2, 1) (r wh)

part2 :: Solver
part2 = show . gps . idtrace . uncurry (foldl' move) . idtrace . fixRobot . parseInput2 . splitOn [""] . lines

