module Main where

import Data.List

preambleLength = 25

everySum [n] = []
everySum (n:numbers) = (map (n +) numbers) ++ everySum numbers

checkWrong _ [] = error "nothing wrong here"
checkWrong preamble (n:actual) | elem n $ everySum preamble = checkWrong ((tail preamble) ++ [n]) actual
                               | otherwise = n

findWrong numbers = checkWrong (take preambleLength numbers) (drop preambleLength numbers)

main = do
  cnt <- getContents
  print $ findWrong $ map read $ lines cnt

