module Main where

-- import Data.List

import Debug.Trace
idtrace x = trace (show x) x

matchParens '(' = ')'
matchParens '{' = '}'
matchParens '[' = ']'
matchParens '<' = '>'

data SyntaxResult = Valid | Incomplete [Char] | Corrupt Char
  deriving (Show, Eq, Ord)

parensStack [] [] = Valid
parensStack stack [] = Incomplete $ map matchParens stack
parensStack stack (p:rest) | elem p "({[<" = parensStack (p:stack) rest
                           | p == matchParens (head stack) = parensStack (tail stack) rest
                           | otherwise = Corrupt p

score r (Incomplete []) = r
score r (Incomplete (')':rest)) = score (5 * r + 1) (Incomplete rest)
score r (Incomplete (']':rest)) = score (5 * r + 2) (Incomplete rest)
score r (Incomplete ('}':rest)) = score (5 * r + 3) (Incomplete rest)
score r (Incomplete ('>':rest)) = score (5 * r + 4) (Incomplete rest)
score r _  = r

middle ls = ls !! (length ls `div` 2)

main = do
  cnt <- getContents
  print $ middle $ sort $ filter (/= 0) $ map (score 0 . parensStack []) $ lines cnt

