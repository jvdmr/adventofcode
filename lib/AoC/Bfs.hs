{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC.Bfs
  ( bfs
  , bfsDisconnected
  , bfsState
  ) where

bfs :: (Eq a) => (a -> [a]) -> a -> [(a, Int)]
bfs neighbors start = bfs' [start] [(start, 0)]
  where bfs' _ [] = []
        bfs' seen (node@(n, d):queue) = node:let nbs = filter (flip notElem seen) $ neighbors n in bfs' (nbs ++ seen) $ queue ++ map (flip (,) (d + 1)) nbs

bfsDisconnected :: (Eq a) => (a -> [a]) -> [a] -> [[(a, Int)]]
bfsDisconnected neighbors starts = bfs' [] starts
  where bfs' results [] = results
        bfs' results (start:starts) = let newresult = bfs neighbors start
                                          in bfs' (newresult:results) $ filter (flip notElem $ map fst newresult) starts

bfsState :: (Eq a) => b -> (b -> a -> (b, [a])) -> ([a] -> [a]) -> [a] -> [a] -> (b, [a])
bfsState state _ _ result [] = (state, result)
bfsState state neighbors prune result (node:queue) = bfsState state' neighbors prune (node:result) $ prune (queue ++ filter (flip notElem (queue ++ result)) neighbors')
  where (state', neighbors') = neighbors state node

