{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC2025.Day11
  ( part1
  , part2
  , tests
  ) where

import Prelude hiding (foldl) -- use foldl' instead of foldl
import qualified Data.Map as M (findWithDefault, Map, fromList, keys, (!), insert, empty)

import AoC (Solver, Tests)
import AoC.Bfs (bfsAdvanced, ignoreSeen, NeighborFunction, pathTo)
import AoC.Util (multiply, zipTailWith)

type Network = M.Map String [String]
type Path = [String]

constructGraph :: [[String]] -> Network
constructGraph = M.fromList . map node
  where node (k:vs) = (init k, vs)

outputs :: Network -> NeighborFunction Path
outputs network = pathTo $ flip (M.findWithDefault []) network

findPaths :: String -> String -> Network -> [(Path, Int)]
findPaths from to network = filter ((== to) . head . fst) $ bfsAdvanced ignoreSeen (outputs network) [from]

part1 :: Solver
part1 = show . length . findPaths "you" "out" . constructGraph . map words . lines

-- There are no paths from dac to fft in the input, so I'm just writing code finding paths from fft to dac

reverseNetwork :: Network -> Network
reverseNetwork network = foldl' update M.empty $ concat $ map reverseEdge $ M.keys network
  where update rn (k, v) = M.insert k (v:(M.findWithDefault [] k rn)) rn
        reverseEdge k = map (flip (,) k) $ network M.! k

countPathsTo :: Network -> String -> String -> Int
countPathsTo network from = (M.!) network'
  where network' = M.insert from 1 $ M.fromList $ [(k, sum $ map (flip (M.findWithDefault 0) network') $ network M.! k) | k <- M.keys network]

findPathsAlong :: [String] -> Network -> Int
findPathsAlong nodes network = multiply $ zipTailWith (countPathsTo $ reverseNetwork network) nodes

part2 :: Solver
part2 = show . findPathsAlong ["svr", "fft", "dac", "out"] . constructGraph . map words . lines

tests :: Tests
tests =
  [ show . length . lines
  , show . length . findPaths "svr" "out" . constructGraph . map words . lines
  , show . length . findPaths "dac" "fft" . constructGraph . map words . lines
  , show . length . findPaths "fft" "dac" . constructGraph . map words . lines
  ]

