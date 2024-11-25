module Main where

import Data.Char
-- import Data.List
import Data.List.Split

import Debug.Trace
idtrace x = trace (show x) x
ftrace f x = trace (show $ f x) x
strace :: State -> State
strace x@((_, m), _) = trace (concat $ map (++ "\n") m) x

type Coord = (Int, Int)

instance Num Coord where
  (+) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
  negate _ = undefined
  (*) _ _ = undefined
  fromInteger _ = undefined
  abs _ = undefined
  signum _ = undefined

modC :: Coord -> Coord -> Coord
modC (x, y) (mx, my) = (mod x mx, mod y my)

type Tile = Char

type Map = (Coord, [[Tile]])

setXY :: Map -> Coord -> Tile -> Map
setXY (mxy, m) (x, y) t = (mxy, take y m ++ [take x my ++ [t] ++ drop (x + 1) my] ++ drop (y + 1) m)
  where my = m !! y

loc :: Map -> Coord -> Tile
loc (_, m) (x, y) = m !! y !! x

type Instruction = String
type Instructions = [Instruction]

parseInstructions :: String -> Instructions
parseInstructions = split (oneOf "RL")

fill :: [String] -> Map
fill m = ((mx', my'), m')
  where fill' s = " " ++ s ++ take (mx - length s) (repeat ' ') ++ " "
        mx = maximum $ map length m
        my = length m
        filler = [take (mx + 2) $ repeat ' ']
        m' = filler ++ map fill' m ++ filler
        mx' = (length $ head m') - 1
        my' = (length m') - 1

parseInput :: [[String]] -> (Map, Instructions)
parseInput [m, p] = (fill m, parseInstructions $ head p)

right :: Coord -> Coord
right (1, 0) = (0, 1)
right (0, 1) = (-1, 0)
right (-1, 0) = (0, -1)
right (0, -1) = (1, 0)

left :: Coord -> Coord
left (1, 0) = (0, -1)
left (0, 1) = (1, 0)
left (-1, 0) = (0, 1)
left (0, -1) = (-1, 0)

type Orientation = Coord
type Position = (Coord, Orientation)
type State = (Map, Position)

startState :: (Map, Instructions) -> (State, Instructions)
startState (m, ins) = ((setXY m (x, 1) 'x', ((x, 1), (1, 0))), ins)
  where x = fst $ head $ dropWhile (flip elem " #" . snd) $ zip [0..] $ head $ tail $ snd m

inf :: [a] -> [a]
inf l = l ++ inf l

path m@((mx, _), _) (x, y) (ox, 0) | ox > 0 = dropWhile (/= (x, y)) $ inf line
                                   | otherwise = dropWhile (/= (x, y)) $ inf $ reverse line
  where line = [(x', y) | x' <- [0..mx], elem (loc m (x', y)) "#.x"]
path m@((_, my), _) (x, y) (0, oy) | oy > 0 = dropWhile (/= (x, y)) $ inf line
                                   | otherwise = dropWhile (/= (x, y)) $ inf $ reverse line
  where line = [(x, y') | y' <- [0..my], elem (loc m (x, y')) "#.x"]

move :: Map -> Instruction -> Coord -> Orientation -> Coord
move _ "R" p _ = p
move _ "L" p _ = p
move m sn p o | isDigit $ head sn = last $ takeWhile ((flip elem ".x") . loc m) $ take (n + 1) $ path m p o
  where n = read sn

turn :: String -> Orientation -> Orientation
turn "R" = right
turn "L" = left
turn _ = id

go :: (State, Instructions) -> State
go (s, []) = strace s
go (s@(m, (l, o)), (i:is)) = go ((setXY m l' 'x', (l', o')), is)
  where l' = move m i l o
        o' = turn i o

num :: Orientation -> Int
num (1, 0) = 0
num (0, 1) = 3
num (-1, 0) = 2
num (0, -1) = 1

password :: State -> Int
password (_, ((x, y), o)) = 1000 * y + 4 * x + num o

main = do
  cnt <- getContents
  print $ password $ go $ startState $ parseInput $ splitOn [""] $ lines cnt

