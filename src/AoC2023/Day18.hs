{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC2023.Day18
  ( part1
  , part2
  ) where

import Data.List.Split (splitOn)

import AoC (Solver, hexToDec)
import AoC.Grid

type GCoord = Coord Int

data Instruction = Dig Direction Int
  deriving (Show, Eq)

parseInstruction :: String -> Instruction
parseInstruction s = Dig (read d) (read l)
  where [d, l, _] = splitOn " " s

parseInstruction2 :: String -> Instruction
parseInstruction2 s = Dig (dir ds) (hexToDec ls)
  where cstr = init $ tail $ tail $ last $ splitOn " " s
        ds = read [last cstr]
        ls = init cstr
        dir 0 = R
        dir 1 = D
        dir 2 = L
        dir 3 = U

type Instructions = [Instruction]

goFar :: Direction -> Int -> GCoord -> GCoord
goFar D n = add (0, n)
goFar R n = add (n, 0)
goFar U n = add (0, -n)
goFar L n = add (-n, 0)

loopL :: GCoord -> Instruction -> Instructions -> [GCoord]
loopL c _ [_] = [c]
loopL c (Dig pd _) (i@(Dig d l):rest@((Dig nd _):_)) = c:loopL c' i rest
  where c' = goFar d (l + doubleTurn pd d nd) c
        doubleTurn R D L = 1
        doubleTurn U R D = 1
        doubleTurn L U R = 1
        doubleTurn D L U = 1
        doubleTurn D R U = -1
        doubleTurn R U L = -1
        doubleTurn U L D = -1
        doubleTurn L D R = -1
        doubleTurn _ _ _ = 0

loopR :: GCoord -> Instruction -> Instructions -> [GCoord]
loopR c _ [_] = [c]
loopR c (Dig pd _) (i@(Dig d l):rest@((Dig nd _):_)) = c:loopR c' i rest
  where c' = goFar d (l + doubleTurn pd d nd) c
        doubleTurn R D L = -1
        doubleTurn U R D = -1
        doubleTurn L U R = -1
        doubleTurn D L U = -1
        doubleTurn D R U = 1
        doubleTurn R U L = 1
        doubleTurn U L D = 1
        doubleTurn L D R = 1
        doubleTurn _ _ _ = 0

shoelace :: [GCoord] -> Int
shoelace cs = (a - b) `div` 2
  where a = sum $ zipWith (*) (map fst cs) (map snd cs')
        b = sum $ zipWith (*) (map fst cs') (map snd cs)
        cs' = tail cs ++ [head cs]

part1 :: Solver
part1 = show . area . map parseInstruction . lines
  where area ins = max (a ins) (b ins)
        a ins = shoelace $ loopL (0, 0) (last ins) ins
        b ins = shoelace $ loopR (0, 0) (last ins) ins

part2 :: Solver
part2 = show . area . map parseInstruction2 . lines
  where area ins = max (a ins) (b ins)
        a ins = shoelace $ loopL (0, 0) (last ins) ins
        b ins = shoelace $ loopR (0, 0) (last ins) ins

