module Main where

import Data.Char
import Data.List
import Data.List.Split

import Debug.Trace
idtrace x = trace (show x) x
lsttrace x = trace (foldl (++) "" $ map ((++"\n") . show) x) x

type Stack = [Char]
parseStacks :: [String] -> [Stack]
parseStacks ls = lsttrace $ "":(map (tail . filter (/= ' ')) $ filter ((/= ' ') . head) $ map reverse $ transpose ls)

type Instruction = (Int, Int, Int)

parseInstruction :: String -> Instruction
parseInstruction = instruction . filter isDigit
  where instruction ns = (read $ init $ init ns, read [last $ init ns], read [last ns])

parseInstructions ls = map parseInstruction ls

move :: [Stack] -> Int -> Int -> Int -> [Stack]
move stacks n from to | from < to = (take from stacks) ++ [reverse $ drop n $ reverse $ stacks !! from] ++ (take (to - from - 1) $ drop (from + 1) stacks) ++ [(stacks !! to) ++ (reverse $ take n $ reverse $ stacks !! from)] ++ (drop (to + 1) stacks)
                      | otherwise = (take to stacks) ++ [(stacks !! to) ++ (reverse $ take n $ reverse $ stacks !! from)] ++ (take (from - to - 1) $ drop (to + 1) stacks) ++ [reverse $ drop n $ reverse $ stacks !! from] ++ (drop (from + 1) stacks)

executeInstructions :: [Stack] -> [Instruction] -> [Stack]
executeInstructions stacks [] = stacks
executeInstructions stacks ((n, from, to):ins) = executeInstructions (move stacks n from to) ins

solvePuzzle ls = executeInstructions (parseStacks stacks) $ parseInstructions instructions
  where [stacks, instructions] = splitOn [""] ls

main = do
  cnt <- getContents
  print $ map last $ tail $ solvePuzzle $ lines cnt

