module Main where

import Data.List
import Data.List.Split (splitOn)

import Debug.Trace
idtrace x = trace (show x) x

flatten :: [[a]] -> [a]
flatten = foldl (++) []

-- not needed here but we're keeping it anyway
-- splitN calculates how many possible ways a number n can be split in m parts, where every part >= 1
splitN :: Int -> Int -> Int
splitN n m = (splitN' !! (m - 1)) !! (n - 1)
  where splitN' = (repeat 1):map (map sum . inits) splitN'

possibilities :: Int -> [String]
possibilities n = possibilities' !! (n - 1)
  where possibilities' = hs:[flatten [map (h:) hs' | h <- "#."] | hs' <- possibilities']
        hs = [[h] | h <- "#."]

expand :: String -> [String]
expand h@('.':_) = [h]
expand h@('#':_) = [h]
expand h@('?':_) = possibilities $ length h

match :: ([Int], String) -> String -> [([Int], String)]
match (ns, r) s = skip (ns, gr)
  where gr = group  $ r ++ s
        skip (nss, []) = [(nss, ".")]
        skip ([], [g]) | '.' == head g = [([], g)]
                       | otherwise = []
        skip ([], _) = []
        skip (n:nss, g:grs) | '.' == head g && [] == grs = [(n:nss, g)]
                            | '.' == head g = skip (n:nss, grs)
                            | n == length g && [] == grs = [(n:nss, g)]
                            | n == length g = skip (nss, grs)
                            | n > length g && [] == grs = [(n:nss, g)]
                            | otherwise = []

count :: [[(a, Int)]] -> [(a, Int)]
count [] = []
count (a:rest) = (fst $ head a, sum $ map snd a):count rest

groupWith :: (Eq b) => (a -> b) -> [a] -> [[a]]
groupWith f = groupBy f'
  where f' a b = f a == f b

sortWith :: (Ord b) => (a -> b) -> [a] -> [a]
sortWith f = sortBy f'
  where f' a b = compare (f a) (f b)

combine :: [(([Int], String), Int)] -> [[String]] -> Int
combine rs [] = sum $ map snd $ filter ((== []) . fst . fst) $ flatten [map (flip (,) n) $ match r "." | (r, n) <- rs]
combine rs (s:ss) = combine rs' ss
  where rs' = count $ groupWith fst $ sortWith fst $ flatten $ flatten [map (map (flip (,) n) . match r) s | (r, n) <- rs]

arrange :: ([String], [Int]) -> Int
arrange (springGroups, ns) = combine [((ns, "."), 1)] $ map expand springGroups

unfold :: String -> ([String], [Int])
unfold s = (springGroups, ns)
  where [springs, nsStr] = splitOn " " s
        springGroups = group springs
        ns = map read $ splitOn "," nsStr

main = do
  cnt <- getContents
  print $ sum $ map (arrange . unfold) $ lines cnt

