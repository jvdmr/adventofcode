module Main where

import Data.List

import Debug.Trace
idtrace x = trace (show x) x

matchParens '(' = ')'
matchParens '{' = '}'
matchParens '[' = ']'
matchParens '<' = '>'

data SyntaxResult = Valid | Incomplete | Corrupt Char
  deriving (Show, Eq, Ord)

parensStack [] [] = Valid
parensStack _ [] = Incomplete
parensStack stack (p:rest) | elem p "({[<" = parensStack (p:stack) rest
                           | p == matchParens (head stack) = parensStack (tail stack) rest
                           | otherwise = Corrupt p

score (Corrupt ')') = 3
score (Corrupt ']') = 57
score (Corrupt '}') = 1197
score (Corrupt '>') = 25137
score _ = 0

main = do
  cnt <- getContents
  print $ foldl (+) 0 $ map (score . parensStack []) $ lines cnt

