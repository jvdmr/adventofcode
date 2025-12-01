{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC2025.Day01
  ( part1
  , part2
  , tests
  ) where

import Prelude hiding (foldl, Either(..)) -- use foldl' if you need foldl

import AoC (Solver, Tests)
import AoC.Util (ignoreInput)

data Instruction = Left Int | Right Int
  deriving (Eq, Show)

readInstruction :: [Char] -> Instruction
readInstruction ('L':num) = Left $ read num
readInstruction ('R':num) = Right $ read num

type PasswordState = (Int, Int)  -- position, # zeroes encountered

apply :: Int -> Instruction -> Int
apply a (Right b) = a + b
apply 0 (Left b) = -b
apply a (Left b) = a - 100 - b  -- make sure passing 0 is counted when going left

countZero1 :: Int -> Int
countZero1 i | mod i 100 == 0 = 1
             | otherwise = 0

rotate :: (Int -> Int) -> PasswordState -> Instruction -> PasswordState
rotate countZero (pos, zeroes) inst = (mod r 100, zeroes + countZero r)
  where r = apply pos inst

countZeroes :: (Int -> Int) -> PasswordState -> [Instruction] -> PasswordState
countZeroes countZero state instructions = foldl' (rotate countZero) state instructions

part1 :: Solver
part1 = show . snd . countZeroes countZero1 (50, 0) . map readInstruction . lines

countZero2 :: Int -> Int
countZero2 0 = 1
countZero2 i = div (abs i) 100

part2 :: Solver
part2 = show . snd . countZeroes countZero2 (50, 0) . map readInstruction . lines

tests :: Tests
tests =
  [ ignoreInput $ show $ countZeroes countZero2 (50, 0) [Right 1000]
  , ignoreInput $ show $ countZeroes countZero2 (50, 0) [Left 1000]
  ]

