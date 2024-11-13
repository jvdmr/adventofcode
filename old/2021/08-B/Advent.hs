module Main where

import Data.List hiding (empty, insert)
import Data.List.Split
import Data.Map (insert, (!), keys)
import qualified Data.Map as M

import Debug.Trace
idtrace x = trace (show x) x

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

uniq [] = []
uniq [a] = [a]
uniq (a:b:rst) | a == b = uniq (b:rst)
               | otherwise = a:uniq (b:rst)

analyze :: M.Map String Int -> [String] -> M.Map String Int
analyze m [] = m
analyze m (s:ss) | not (elem 3 (keys fm)) && length s == 5 && isSubsequenceOf (fm ! 1) s = analyze (insert s 3 m) ss
                 | elem 3 (keys fm) && length s == 6 && isSubsequenceOf (fm ! 3) s = analyze (insert s 9 m) ss
                 | elem 3 (keys fm) && length s == 6 && not (isSubsequenceOf (fm ! 3) s) && isSubsequenceOf (fm ! 1) s = analyze (insert s 0 m) ss
                 | elem 3 (keys fm) && length s == 6 && not (isSubsequenceOf (fm ! 3) s) && not (isSubsequenceOf (fm ! 1) s) = analyze (insert s 6 m) ss
                 | elem 9 (keys fm) && not (elem 5 (keys fm)) && length s == 5 && isSubsequenceOf s (fm ! 9) = analyze (insert s 5 m) ss
                 | elem 9 (keys fm) && not (elem 2 (keys fm)) && length s == 5 && not (isSubsequenceOf (fm ! 1) s) && not (isSubsequenceOf s (fm ! 9)) = analyze (insert s 2 m) ss
                 | otherwise = analyze m (ss ++ [s])
                 where fm = M.fromList $ map swap $ M.assocs m

compileKb :: M.Map String Int -> [String] -> M.Map String Int
compileKb m [] = m
compileKb m (s:ss) | length s == 2 = compileKb (insert s 1 m) ss
                   | length s == 3 = compileKb (insert s 7 m) ss
                   | length s == 4 = compileKb (insert s 4 m) ss
                   | length s == 7 = compileKb (insert s 8 m) ss
                   | M.size m >= 4 = analyze m (s:ss)
                   | otherwise = compileKb m (ss ++ [s])

guess :: [[String]] -> Int
guess [d, o] = read $ foldl (++) "" $ map (show . (kb !)) o
  where kb = compileKb M.empty $ uniq $ sort d

process :: String -> Int
process = guess . map (map sort . splitOn " ") . splitOn " | "

main = do
  cnt <- getContents
  print $ foldl (+) 0 $ map process $ lines cnt

