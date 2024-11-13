module Main where

import Debug.Trace
import Data.List
import Data.Array.IO
import Data.Array.Unboxed
import Control.Monad

idtrace x = trace (show x) x
ntrace n x = trace (show n) x
-- infotrace n i a x = trace (show n ++ " / " ++ show i ++ " / " ++ show taken) x
--   where taken = i:(take 4 $ takeF i)
--         takeF y = (a ! y):takeF (a ! y)
movetrace i dest x = trace ("--> " ++ show i ++ " / " ++ show dest ++ " / " ++ show x) x

move :: Int -> Int -> IOUArray Int Int -> IO ()
move i dest arr = do
  writeArray arr i nn
  writeArray arr l after
  writeArray arr dest n
  where n = arr ! i
        l1 = arr ! n
        l = arr ! l1
        nn = arr ! l
        after = arr ! dest

takeF :: IOUArray Int Int -> Int -> [Int]
takeF a y = (a ! y):takeF a (a ! y)

crabcups :: Int -> Int -> IOUArray Int Int -> IO (IOUArray Int Int)
crabcups 0 _ a = return a
crabcups n i a = do
  move i (findDest taken $ ntrace n (i - 1)) a
--   a' <- move i (findDest taken (i - 1)) a
  crabcups (n - 1) (a ! i) a
  where taken = take 3 $ takeF a i

findDest :: [Int] -> Int -> Int
findDest taken x | x == 0 = findDest taken 1000000
                 | elem x taken = findDest taken (x - 1)
                 | otherwise = x

fillList :: Int -> [Int] -> IO (IOUArray Int Int)
fillList max ls = do
  arr <- newListArray (1, max) (map snd $ sortBy comp $ take max $ zip infl $ tail infl)
  writeArray arr 1000000 (head ls)
  return arr
  where nxt = (head $ reverse $ sort ls) + 1
        infl = ls ++ [nxt..max] ++ infl
        comp a b = compare (fst a) (fst b)

calculate :: IOUArray Int Int -> Int
calculate a = b * c
  where b = a ! 1
        c = a ! b

main = do
  cnt <- getContents
  fullList <- fillList 1000000 $ map (read . (:[])) $ head $ lines cnt
  result <- crabcups 10000000 (read $ (:[]) $ head $ cnt) fullList
  print $ calculate result

