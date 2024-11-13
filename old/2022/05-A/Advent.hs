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

moveX :: [Stack] -> Int -> Int -> [Stack]
moveX stacks from to | from < to = (take from stacks) ++ [init $ stacks !! from] ++ (take (to - from - 1) $ drop (from + 1) stacks) ++ [(stacks !! to) ++ [last $ stacks !! from]] ++ (drop (to + 1) stacks)
                     | otherwise = (take to stacks) ++ [(stacks !! to) ++ [last $ stacks !! from]] ++ (take (from - to - 1) $ drop (to + 1) stacks) ++ [init $ stacks !! from] ++ (drop (from + 1) stacks)

move stacks 0 _ _ = stacks
move stacks n from to = move (moveX stacks from to) (n - 1) from to

executeInstructions :: [Stack] -> [Instruction] -> [Stack]
executeInstructions stacks [] = stacks
executeInstructions stacks ((n, from, to):ins) = executeInstructions (move stacks n from to) ins

solvePuzzle ls = executeInstructions (parseStacks stacks) $ parseInstructions instructions
  where [stacks, instructions] = splitOn [""] ls

main = do
  cnt <- getContents
  print $ map last $ tail $ solvePuzzle $ lines cnt

