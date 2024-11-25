module Main where

-- import Data.List
import Data.List.Split
import Data.Map (empty, (!), insert, Map, member, elems, findWithDefault, fromList, keys)
import qualified Data.Map as M

import Debug.Trace
idtrace x = trace (show x) x

p [a, b] = (a, head b)

polymerize (template:"":rulesInput) = (iterate step template) !! 10
  where step [a] = [a]
        step (a:b:rest) | elem [a, b] $ keys rules = a:rules ! [a, b]:step (b:rest)
                        | otherwise = a:step (b:rest)
        rules = fromList $ map (p . splitOn " -> ") rulesInput

compareLength a b = compare (length b) (length a)

checksum :: [Char] -> Int
checksum polymer = lmost - lleast
  where spoly = sortBy compareLength $ group $ sort polymer
        lmost = length $ head spoly
        lleast = length $ last spoly

main = do
  cnt <- getContents
  print $ checksum $ idtrace $ polymerize $ lines cnt

