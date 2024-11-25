module Main where

-- import Data.List
import Data.Ord

import Debug.Trace
idtrace x = trace (show x) x
ftrace f x = trace (f x) x

uniq [] = []
uniq [a] = [a]
uniq (a:b:rst) | a == b = uniq (b:rst)
               | otherwise = a:uniq (b:rst)

qe :: [Int] -> Int
qe ps = foldl (*) 1 ps

bin :: ((Int, [Int], [Int]) -> Bool) -> [Int] -> [(Int, [Int], [Int])]
bin _ [] = [(0, [], [])]
bin deny (a:as) = (map dropA rest) ++ filter (not . deny) (map addA rest)
  where rest = bin deny as
        addA (s, l, d) = (s + a, a:l, d)
        dropA (s, l, d) = (s, l, a:d)

configurations :: [Int] -> [[Int]]
configurations ls = idtrace $ map fst $ filter ((/= []) . snd . sndbin . presents . head . snd) $ filter ((/= []) . snd) $ map sndbin $ takeWhile ((== shortest) . length . fst) combinations
  where lim = sum ls `div` 4
        combinations = sortBy (comparing (length . fst)) $ map presents $ filter isLim $ bin deny ls
        shortest = length $ fst $ head combinations
        presents (_, l, d) = (l, d)
        isLim (s, _, _) = s == lim
        headOrNothing [] = []
        headOrNothing (a:_) = [a]
        sndbin (l, d) = (l, headOrNothing $ filter isLim $ bin deny d)
        deny (s, l, _) = s > lim

main = do
  cnt <- getContents
  print $ qe $ head $ sortBy (comparing qe) $ configurations $ map (read :: String -> Int) $ lines cnt

