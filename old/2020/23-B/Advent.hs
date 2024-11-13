module Main where

import Debug.Trace
import Data.List hiding (insert)
import qualified Data.IntMap.Strict as M

idtrace x = trace (show x) x
infotrace a b c x = trace (show a ++ " / " ++ show b ++ " / " ++ show c) x

-- (!) m k | M.member k m = m M.! k
--         | otherwise = k + 1 
(!) = (M.!)

myempty = M.empty

insert = M.insert

-- crabcups (i, m) = infotrace '>' i (n:b:l:nn:dest:after:[]) (nn, m')
crabcups (i, m) = (nn, m')
--   where m' = insert i nn $ insert l after $ insert dest n m
  where m' = M.union (M.fromList [(i, nn), (l, after), (dest, n)]) m
        n = m ! i
        b = m ! n
        l = m ! b
        nn = m ! l
        after = m ! dest
        tas = n:b:l:[]
        dest = findDest (i - 1)
        findDest n | n == 0 = findDest 1000000
                   | not $ elem n tas = findDest (n - 1)
                   | otherwise = n

createMap m max [a] = insert a (newMax + 1) m
  where newMax | max < a = a
               | otherwise = max
createMap m max (a:b:rst) = createMap (insert a b m) newMax (b:rst)
  where newMax | max < a = a
               | otherwise = max

newMap (a:rst) = createMap (insert 1000000 a myempty) a (a:rst ++ [10..999999])

calculate (_, m) = (m ! 1) * (m ! (m ! 1))

main = do
  cnt <- getContents
  print $ calculate $ iterate crabcups ((read $ (head cnt):[]), newMap $ map (read . (:[])) $ head $ lines cnt) !! 10000000

