module Main where

import Data.List
import Data.Strings

niceRepeats [_, _] = False
niceRepeats (a:b:c:rst) = a == c || niceRepeats (b:c:rst)

nicePairs [_, _, _] = False
nicePairs s = foundPair || nicePairs (tail s)
  where foundPair = "" /= snd (strBreak h t)
        h = take 2 s
        t = drop 2 s

naughtyOrNice s = niceRepeats s && nicePairs s

main = do
  cnt <- getContents
  print $ length $ filter naughtyOrNice $ lines cnt

