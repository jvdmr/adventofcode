module Main where

import Debug.Trace
import Data.List hiding (insert)
import Data.HashMap hiding (map)
import Text.PrettyPrint hiding (empty)

validNext a b = a + 3 >= b

-- findNextCombos (a:[]) = []
-- findNextCombos (a:b:rst) | validNext a b = (b:rst):findNextCombos (a:rst)
--                          | otherwise = []

-- countPaths :: [Integer] -> Integer
-- countPaths graph = last $ map cp unrolled
-- countPaths graph = foldl (\m l -> insert l (cp l) m) empty unrolled ! graph
--   where cp [n] = 1
--         cp g = foldl (+) 0 $ map countPaths $ findNextCombos g
--         unrolled = reverse $ unroll graph
--         unroll [] = []
--         unroll (a:rst) = (a:rst):unroll rst

findNextCombos (a:[]) = []
findNextCombos (a:b:rst) | validNext a b = b:findNextCombos (a:rst)
                         | otherwise = []

countPaths :: Map Integer Integer -> [Integer] -> Integer
countPaths _ [_] = 1
countPaths hm graph = foldl (+) 0 $ map helper $ findNextCombos graph
  where helper n = mem ! n
        mem = foldl (\m l -> findOrInsert (head l) (countPaths m l) m) hm unrolled
        unrolled = reverse $ unroll graph
        unroll [] = []
        unroll (a:rst) = (a:rst):unroll rst

findOrInsert k v m | member k m = m
                   | otherwise = insert k v m

idtrace x = trace (show x) x
ptrace s x = trace (s ++ show x) x

main = do
  cnt <- getContents
--   print $ foldl ($$) (text "") $ map (text . show) $ createGraph $ (0:) $ sort $ map read $ lines cnt
--   print $ countPaths $ createGraph $ (0:) $ sort $ map read $ lines cnt
--   print $ (0:) $ sort $ map read $ lines cnt
  print $ countPaths empty $ (0:) $ sort $ map read $ lines cnt

