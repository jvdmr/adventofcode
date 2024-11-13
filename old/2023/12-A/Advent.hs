module Main where

import Data.List
import Data.List.Split (splitOn)

import Debug.Trace
idtrace x = trace (show x) x

ftrace :: (a -> String) -> a -> a
ftrace f x = trace (f x) x

-- not needed here but we're keeping it anyway
-- splitN calculates how many possible ways a number n can be split in m parts, where every part >= 1
splitN :: Int -> Int -> Int
splitN n m = (splitN' !! (m - 1)) !! (n - 1)
  where splitN' = (repeat 1):map (map sum . inits) splitN'

match :: (Char, Char) -> Bool
match ('?', _) = True
match (a, b) = a == b

matchFill :: String -> String -> Bool
matchFill springs = all match . zipWith (,) springs

generate :: [(String, [String])] -> Int
generate [] = 0
generate ((springs, []):rest) | matchFill springs suffix = 1 + generate rest
                              | otherwise = generate rest
  where suffix = take (length springs) $ repeat '.'
generate (("", _):rest) = generate rest
generate ((springs, (s:pattern)):rest) | maxFill < 0 = generate rest
                                       | otherwise = generate $ [(drop p springs, pattern) | p <- map length $ filter (matchFill springs) fills] ++ rest
  where baseLength = sum $ map length (s:pattern)
        maxFill = length springs - baseLength
        fills = map ((++ s) . (flip take $ repeat '.')) [0..maxFill]

arrange :: (String, [Int]) -> Int
arrange (springs, ns) = generate $ [(springs, head pattern:(map ('.':) $ tail pattern))]
  where pattern = map (flip take $ repeat '#') ns

unfold :: String -> (String, [Int])
unfold s = (springs, ns)
  where [springs, nsStr] = splitOn " " s
        ns = map read $ splitOn "," nsStr

main = do
  cnt <- getContents
  print $ sum $ map (arrange . unfold) $ lines cnt

