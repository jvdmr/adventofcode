module Main where

-- import Data.List
import Data.List.Split

import Debug.Trace
idtrace x = trace (show x) x
ftrace f x = trace (show $ f x) x

type File = [(Int, Int)]

move :: Int -> File -> File
move n f = f''
  where [a, b@[(_, m)], c] = split (whenElt ((== n) . fst)) f
        f' = c ++ a
        lf = length f'
        m' = mod m lf
        (a', c') = splitAt m' f'
        f'' = a' ++ b ++ c'

coords f = sum $ idtrace $ map (f' !!) [1000, 2000, 3000]
  where f' = concat $ repeat f''
        f'' = map snd $ b ++ c ++ a
        [a, b, c] = split (whenElt ((== 0) . snd)) f

mix :: File -> Int
mix file = coords $ mix' 0 file
  where mix' n f | n == fl = f
                 | otherwise = mix' (n + 1) $ move n f
        fl = length file

main = do
  cnt <- getContents
  print $ mix $ zip [0..] $ map read $ lines cnt

