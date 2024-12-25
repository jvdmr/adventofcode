{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC2024.Day23
  ( part1
  , part2
  , tests
  ) where

import Prelude hiding (foldl)
import Data.List (sort, intercalate, nub, sortBy)
import Data.List.Split (splitOn)
import Data.Ord (comparing)
import Data.Map ((!))
import qualified Data.Map as M

import AoC (Solver, Tests)
import AoC.Util (($>), unpair, Pair, toList, ($<), trueOrFalse)
import AoC.Trace

type Computer = String
type Network = M.Map Computer [Computer]

addEdge :: Network -> String -> Network
addEdge nw s = M.alter (f a) b $ M.alter (f b) a nw
  where [a, b] = splitOn "-" s
        f n Nothing = Just [n]
        f n (Just ns) = Just $ n:ns

network :: [String] -> Network
network = foldl' addEdge M.empty

triads :: Char -> Network -> [[Computer]]
triads c nw = foldl' f [] $ filter ((==) c . head) $ M.keys nw
  where f seen n = let nbs = nw ! n in nub $ (++) seen $ map (sort . (n:)) $ concat $ map lsts $ filter (not . null . snd) $ map (($>) (filter $ flip elem nbs) . nnbs) nbs
        nnbs n = (n, nw ! n)
        lsts (n, ns) = map ((n:) . (:[])) ns

tests :: Tests
tests =
  [ show . length . lines
  ]

part1 :: Solver
part1 = show . length . ftrace (intercalate "\n" . map show) . triads 't' . network . lines

largestGroup :: Network -> [Computer]
largestGroup nw = sort $ last $ sortBy (comparing length) $ foldl' f [] $ M.keys nw
  where f seen n = let nbs = nw ! n in ([n]:) $ concat $ toList $ map (n:) $< trueOrFalse (all $ flip elem nbs) seen

part2 :: Solver
part2 = show . intercalate "," . largestGroup . network . lines

