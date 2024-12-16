{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC.Bfs
  ( bfs
  , SeenFilter (..)
  , NeighborFunction (..)
  , bfsAdvanced
  , bfsDisconnected
  , bfsPath
  , bfsState
  , ignoreSeen
  , pathTo
  , pathToAdv
  ) where

type SeenFilter a = [a] -> a -> Bool
type NeighborFunction a = a -> [a]

-- Allows custom seenFilter
bfsAdvanced :: SeenFilter a -> NeighborFunction a -> a -> [(a, Int)]
bfsAdvanced seenFilter neighbors start = bfs' [start] [(start, 0)]
  where bfs' _ [] = []
        bfs' seen (node@(n, d):queue) = node:let nbs = filter (seenFilter seen) $ neighbors n in bfs' (nbs ++ seen) $ (filter (seenFilter nbs . fst) queue) ++ map (flip (,) (d + 1)) nbs

-- Standard BFS
bfs :: (Eq a) => NeighborFunction a -> a -> [(a, Int)]
bfs neighbors start = bfsAdvanced (flip notElem) neighbors start

-- Wraps a standard neighbors function in one that builds a path with a custom seenFilter
pathToAdv :: SeenFilter a -> NeighborFunction a -> NeighborFunction [a]
pathToAdv seenFilter neighbors as@(a:_) = map (flip (:) as) $ filter (seenFilter as) $ neighbors a

-- Wraps a standard neighbors function in one that builds a path
pathTo :: (Eq a) => NeighborFunction a -> NeighborFunction [a]
pathTo = pathToAdv (flip notElem)

ignoreSeen :: SeenFilter a
ignoreSeen _ _ = True

{-# INLINE ignoreSeen #-}

-- BFS that constructs the shortest (unweighted) path for every node to the starting node
bfsPath :: (Eq a) => NeighborFunction a -> a -> [([a], Int)]
bfsPath neighbors start = bfsAdvanced ignoreSeen (pathTo neighbors) [start]

-- BFS that finds all disconnected parts of a graph
bfsDisconnected :: (Eq a) => NeighborFunction a -> [a] -> [[(a, Int)]]
bfsDisconnected neighbors starts = bfs' [] starts
  where bfs' results [] = results
        bfs' results (start:starts) = let newresult = bfs neighbors start
                                          in bfs' (newresult:results) $ filter (flip notElem $ map fst newresult) starts

-- BFS that keeps track of a state while traversing the graph
bfsState :: (Eq a) => b -> (b -> a -> (b, [a])) -> ([a] -> [a]) -> [a] -> [a] -> (b, [a])
bfsState state _ _ result [] = (state, result)
bfsState state neighbors prune result (node:queue) = bfsState state' neighbors prune (node:result) $ prune (queue ++ filter (flip notElem (queue ++ result)) neighbors')
  where (state', neighbors') = neighbors state node

