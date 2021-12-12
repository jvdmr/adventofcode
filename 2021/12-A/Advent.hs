module Main where

import Data.Char
import Data.List hiding (empty, insert)
import Data.List.Split
import Data.Map (empty, (!), insert, Map, member, elems, findWithDefault, fromList, keys)
import qualified Data.Map as M

import Debug.Trace
idtrace x = trace (show x) x

flatten :: [[a]] -> [a]
flatten = foldl (++) []

type Graph = Map String [String]

insertG :: String -> String -> Graph -> Graph
insertG k a g = insert k (a:findWithDefault [] k g) g

parseEdge :: Graph -> String -> Graph
parseEdge g e = insertG a b $ insertG b a g
  where [a, b] = splitOn "-" e

validPath :: [String] -> Bool
validPath p = "end" == head p

findPaths :: Graph -> [String] -> [[String]]
findPaths g p@("end":_) = [p]
findPaths g p@(n:_) = filter validPath $ flatten $ map (findPaths g . (:p)) $ filter seenSmall $ g ! n
  where seenSmall i = (isUpper $ head i) || (not $ elem i p)

main = do
  cnt <- getContents
  print $ length $ flip findPaths ["start"] $ foldl parseEdge empty $ lines cnt

