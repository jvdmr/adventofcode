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
import AoC.Memoize (MemoizeableFunction, memoize)
import AoC.Trace (idtrace)

type CheckFunction = MemoizeableFunction (String, [Int]) Int

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

check' :: String -> [Int] -> Int
check' springs pattern = check'' (springs, pattern)
  where check'' (s', p') = [[check check'' (s, p) | p <- reverse $ tails pattern] | s <- reverse $ tails springs] !! length s' !! length p'
--   where check'' = memoize check index combined
--         combined = combineLimited (reverse $ tails springs) (reverse $ tails pattern)
--         index (a, b) = M.fromList (zip combined' [0..]) ! (length a, length b)
--         combined' = [(length a, length b) | (a, b) <- combined]

unfold :: Int -> String -> (String, [Int])
unfold i s = (intercalate "?" $ take i $ repeat springs, concat $ take i $ repeat ns)
  where [springs, nsStr] = splitOn " " s
        ns = map read $ splitOn "," nsStr

part1 :: Solver
part1 = show . sum . map (uncurry check' . unfold 1) . lines

part2 :: Solver
part2 = show . sum . map (uncurry check' . unfold 5) . lines

