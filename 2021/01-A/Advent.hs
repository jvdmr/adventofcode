module Main where

import Data.List
import Debug.Trace

countInc :: Int -> [Int] -> Int
countInc i [] = i
countInc i [_] = i
countInc i (a:b:ls) | a < b = trace (show b ++ " inc!") $ countInc (i+1) (b:ls)
                    | otherwise = trace (show b) $ countInc i (b:ls)

main = do
  cnt <- getContents
  print $ countInc 0 $ map read $ lines cnt

