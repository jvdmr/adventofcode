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

generate :: Int -> (String, [String]) -> Int
generate result (springs, []) | matchFill springs suffix = result + 1
                              | otherwise = result
  where suffix = take (length springs) $ repeat '.'
generate result ("", _) = result
generate result (springs, (s:pattern)) | maxFill < 0 = result
                                       | otherwise = foldl generate result [(drop p springs, pattern) | p <- map length $ filter (matchFill springs) fills]
  where baseLength = sum $ map length (s:pattern)
        maxFill = length springs - baseLength
        fills = map ((++ s) . (flip take $ repeat '.')) [0..maxFill]

arrange :: (String, [Int]) -> Int
arrange (springs, ns) = generate 0 $ idtrace (springs, head pattern:(map ('.':) $ tail pattern))
  where pattern = map (flip take $ repeat '#') ns

join :: [a] -> [[a]] -> [a]
join i = concat . intersperse i

unfold :: String -> (String, [Int])
unfold s = (springs, concat $ take 5 $ repeat ns)
  where [springsStr, nsStr] = splitOn " " s
        springs = join "?" $ take 5 $ repeat springsStr
        ns = map read $ splitOn "," nsStr

main = do
  cnt <- getContents
  print $ sum $ map (arrange . unfold) $ lines cnt

