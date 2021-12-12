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

uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq [a] = [a]
uniq (a:b:rst) | a == b = uniq (b:rst)
               | otherwise = a:uniq (b:rst)

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
  where seenSmall "start" = False
        seenSmall i = (isUpper $ head i) || length small == length (uniq small) || (not $ elem i p)
        small = sort $ filter (isLower . head) p

main = do
  cnt <- getContents
  print $ length $ flip findPaths ["start"] $ foldl parseEdge empty $ lines cnt

