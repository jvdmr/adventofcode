module Main where

import Data.List
import Data.Strings

vowels = "aeiou"
naughty = ["ab", "cd", "pq", "xy"]

niceVowels s = l >= 3
  where l = length $ filter (flip elem vowels) s

nicePairs [s] = False
nicePairs (a:b:rst) = a == b || nicePairs (b:rst)

notNaughty s = [] == filter ((/=) "" . snd . flip strBreak s) naughty

naughtyOrNice s = niceVowels s && nicePairs s && notNaughty s

main = do
  cnt <- getContents
  print $ length $ filter naughtyOrNice $ lines cnt

