module Main where

-- import Data.List
import Data.List.Split (splitOn)
import Data.Map ((!))
import qualified Data.Map as M

import Debug.Trace
idtrace x = trace (show x) x

type Chain = M.Map String String
type Mapping = M.Map Int (Int, Int)
type Mappings = M.Map String Mapping

findOrId :: Mapping -> Int -> [Int] -> Int
findOrId _ k [] = k
findOrId m k (k':ks) | k >= k' && k <= k' + r = k + diff
                     | otherwise = findOrId m k ks
  where (r, diff) = m ! k'

(!?) :: Mapping -> Int -> Int
(!?) m k = findOrId m k $ M.keys m

expandRanges :: [Int] -> (Int, (Int, Int))
expandRanges [dest, src, r] = (src, (r, dest - src))

parseMappings :: (Chain, Mappings) -> [[String]] -> (Chain, Mappings)
parseMappings (c, ms) [] = (c, ms)
parseMappings (c, ms) ((name:numbers):rest) = parseMappings (c', ms') rest
  where c' = M.insert from to c
        [from, to] = splitOn "-to-" $ head $ splitOn " " name
        ms' = M.insert to numberMap ms
        numberMap = M.fromList $ map (expandRanges . map read . splitOn " ") numbers

followChain :: Chain -> Mappings -> String -> String -> [Int] -> [Int]
followChain chain mappings end current paths | end == current = paths
                                             | otherwise = followChain chain mappings end next $ map ((!?) (mappings ! next)) paths
  where next = chain ! current

findLocation :: [[String]] -> Int
findLocation (seedStr:mapsStrs) = head $ sort $ followChain chain mappings "location" "seed" seeds
  where seeds = map read $ tail $ splitOn " " $ head seedStr
        (chain, mappings) = parseMappings (M.empty, M.empty) mapsStrs

main = do
  cnt <- getContents
  print $ findLocation $ splitOn [""] $ lines cnt

