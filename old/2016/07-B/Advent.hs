module Main where

-- import Data.List
import Data.List.Split

import Debug.Trace
idtrace x = trace (show x) x

evenuneven ls = [map snd $ filter (odd . fst) indexed, map snd $ filter (even . fst) indexed]
  where indexed = zip [1..] ls

bab (a:b:_) = [b, a, b]

abas (a:r@(b:c:_)) | a /= b && a == c = [a, b, c]:abas r
                   | otherwise = abas r
abas _ = []

ababab [abals, babls] = [] /= (filter (\s -> [] /= (filter (isInfixOf s) babls)) $ map bab $ concat $ map abas abals)

ssl = ababab . evenuneven . splitOneOf "[]"

main = do
  cnt <- getContents
  print $ length $ filter ssl $ lines cnt

