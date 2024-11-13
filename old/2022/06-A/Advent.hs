module Main where

import Data.List

import Debug.Trace
idtrace x = trace (show x) x

uniq [] = []
uniq [a] = [a]
uniq (a:b:rst) | a == b = uniq (b:rst)
               | otherwise = a:uniq (b:rst)

findMarker prev (a:rest) | length prev >= 3 && (sort (take 3 prev) == uniq (sort $ take 3 prev)) && (notElem a $ take 3 $ idtrace prev) = length prev + 1
                         | otherwise = findMarker (a:prev) rest

main = do
  cnt <- getContents
  print $ map (findMarker []) $ lines cnt

