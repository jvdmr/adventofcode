module Main where

import Data.List hiding (empty, insert)
import Data.List.Split
import Data.Map (empty, (!), insert, Map, member, elems, findWithDefault, fromList, keys, toList)

import Debug.Trace
idtrace x = trace (show x) x

p [a, b] = (a, head b)

pairs [a, b] = [[a, b]]
pairs (a:b:rest) = [a, b]:pairs (b:rest)

pairify template = fromList $ map (\s -> (head s, length s)) $ group $ sort $ pairs template

summarize [a] = [a]
summarize ((a, i):(b, j):rest) | a == b = summarize $ (a, i + j):rest
                               | otherwise = (a, i):summarize ((b, j):rest)

polymerize (template:"":rulesInput) = depairify (last template) $ (iterate step $ pairify template) !! 40
  where step m = fromList $ summarize $ sort $ applyRules $ toList m
        applyRules [] = []
        applyRules ((s@[a, b], n):rest) = ([a, rules!s], n):([rules!s, b], n):applyRules rest
        rules = fromList $ map (p . splitOn " -> ") rulesInput

depairify l m = count (insert l 1 empty) $ toList m
  where count r [] = toList r
        count r (([a, _], n):rest) = count (insert a (n + findWithDefault 0 a r) r) rest

checksum polymer = (last polymer) - (head polymer)

main = do
  cnt <- getContents
  print $ checksum $ sort $ map snd $ polymerize $ lines cnt

