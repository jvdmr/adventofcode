module Main where

-- import Data.List
import Data.List.Split (splitOn)
import Data.Map ((!))
import qualified Data.Map as M

import Debug.Trace
idtrace x = trace (show x) x
marktrace x = trace ("[ " ++ show x ++ " ]") x

type Chain = M.Map String String
type Mapping = M.Map Int (Int, Int)
type Mappings = M.Map String Mapping

findOrId :: Mapping -> Int -> [Int] -> Int
findOrId _ k [] = k
findOrId m k (k':ks) | k >= k' && k <= k'' = k + diff
                     | otherwise = findOrId m k ks
  where (k'', diff) = m ! k'

(!?) :: Mapping -> Int -> Int
(!?) m k = findOrId m k $ M.keys m

expandRanges :: [Int] -> (Int, (Int, Int))
expandRanges [dest, src, r] = (dest, (dest + r - 1, src - dest))

parseMappings :: (Chain, Mappings) -> [[String]] -> (Chain, Mappings)
parseMappings (c, ms) [] = (c, ms)
parseMappings (c, ms) ((name:numbers):rest) = parseMappings (c', ms') rest
  where c' = M.insert to from c
        [from, to] = splitOn "-to-" $ head $ splitOn " " name
        ms' = M.insert to numberMap ms
        numberMap = M.fromList $ map (expandRanges . map read . splitOn " ") numbers

followChain :: Chain -> Mappings -> String -> String -> Int -> Int
followChain chain mappings end current num | end == current = mappings ! current !? num
                                           | otherwise = followChain chain mappings end (chain ! current) $ (mappings ! current) !? num

parseSeeds :: [Int] -> [(Int, (Int, Int))]
parseSeeds [] = []
parseSeeds (start:range:rest) = (start, (start + range - 1, -(start + range))):parseSeeds rest

findLocation :: [[String]] -> Int
findLocation (seedStr:mapsStrs) = head $ dropWhile ((>= 0) . followHelp) [1..]
  where seeds = M.fromList $ parseSeeds $ map read $ tail $ splitOn " " $ head seedStr
        (chain, mappings) = parseMappings (M.empty, M.empty) mapsStrs
        followHelp = followChain chain (M.insert "seed" seeds mappings) "seed" "location"

main = do
  cnt <- getContents
  print $ findLocation $ splitOn [""] $ lines cnt

