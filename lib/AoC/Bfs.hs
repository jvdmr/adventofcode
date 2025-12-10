{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC.Bfs
  ( bfs
  , UnseenFilter (..)
  , NeighborFunction (..)
  , bfsAdvanced
  , bfsDisconnected
  , bfsPath
  , bfsState
  , ignoreSeen
  , pathTo
  , pathToAdv
  , unseenHeads
  ) where

type UnseenFilter a = [a] -> a -> Bool
type NeighborFunction a = a -> [a]

-- Allows custom unseenFilter
bfsAdvanced :: UnseenFilter a -> NeighborFunction a -> a -> [(a, Int)]
bfsAdvanced unseenFilter neighbors start = bfs' [start] [(start, 0)]
  where bfs' _ [] = []
        bfs' seen (node@(n, d):queue) = node:let nbs = filter (unseenFilter seen) $ neighbors n in bfs' (nbs ++ seen) $ queue ++ map (flip (,) (d + 1)) nbs

-- Standard BFS
bfs :: (Eq a) => NeighborFunction a -> a -> [(a, Int)]
bfs neighbors start = bfsAdvanced (flip notElem) neighbors start

-- Wraps a standard neighbors function in one that builds a path with a custom unseenFilter
pathToAdv :: UnseenFilter a -> NeighborFunction a -> NeighborFunction [a]
pathToAdv unseenFilter neighbors as@(a:_) = map (flip (:) as) $ filter (unseenFilter as) $ neighbors a

-- Wraps a standard neighbors function in one that builds a path
pathTo :: (Eq a) => NeighborFunction a -> NeighborFunction [a]
pathTo = pathToAdv (flip notElem)

ignoreSeen :: UnseenFilter a
ignoreSeen _ _ = True

{-# INLINE ignoreSeen #-}

unseenHeads :: (Eq a) => UnseenFilter [a]
unseenHeads as a = head a `notElem` map head as

-- BFS that constructs the shortest (unweighted) path for every node to the starting node
bfsPath :: (Eq a) => NeighborFunction a -> a -> [([a], Int)]
bfsPath neighbors start = bfsAdvanced unseenHeads (pathTo neighbors) [start]

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

