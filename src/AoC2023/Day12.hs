{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC2023.Day12
  ( part1
  , part2
  ) where

import Data.List
import Data.List.Split (splitOn)
import Data.Map ((!))
import qualified Data.Map as M (fromList)

import AoC (Solver, combineLimited)
import AoC.Memoize

type CheckFunction = ((String, [Int]) -> Int) -> (String, [Int]) -> Int

skip :: CheckFunction
skip checkf ([], p) = checkf ([], p)
skip checkf (s, []) = checkf (s, [])
skip checkf ('?':s, p) = checkf (s, p)
skip checkf ('#':_, _) = 0
skip checkf ('.':s, p) = checkf (s', p)
  where s' = dropWhile (== '.') s

block :: CheckFunction
block checkf (s, 0:p) = skip checkf (s, p)
block checkf ([], p) = checkf ([], p)
block checkf (s, []) = checkf (s, [])
block checkf ('.':_, _) = 0
block checkf (_:s, n:p) = block checkf (s, n - 1:p)

check :: CheckFunction
check checkf ("", []) = 1
check checkf ("", _) = 0
check checkf (s, []) | elem '#' s = 0
                     | otherwise = 1
check checkf (s@('?':_), p) = skip checkf (s, p) + block checkf (s, p)
check checkf (s@('#':_), p) = block checkf (s, p)
check checkf (s@('.':_), p) = skip checkf (s, p)

-- memoization
checkm :: String -> [Int] -> Int
checkm springs pattern = checkm' $ index ! lpair (springs, pattern)
  where checkm' = memoize check'
        springsTails = reverse $ tails springs
        patternTails = reverse $ tails pattern
        combinedTails = combineLimited springsTails patternTails
        combinedLengths = combineLimited (map length springsTails) (map length patternTails)
        index = M.fromList $ zip combinedLengths [0..]
        lpair (a, b) = (length a, length b)
        check' f = check (f . (index !) . lpair) . (combinedTails !!)
-- checkm springs pattern = check' (springs, pattern)
--   where check' (s', p') = [[check check' (s, p) | p <- reverse $ tails pattern] | s <- reverse $ tails springs] !! length s' !! length p'

unfold :: Int -> String -> (String, [Int])
unfold i s = (intercalate "?" $ take i $ repeat springs, concat $ take i $ repeat ns)
  where [springs, nsStr] = splitOn " " s
        ns = map read $ splitOn "," nsStr

part1 :: Solver
part1 = show . sum . map (uncurry checkm . unfold 1) . lines

part2 :: Solver
part2 = show . sum . map (uncurry checkm . unfold 5) . lines

