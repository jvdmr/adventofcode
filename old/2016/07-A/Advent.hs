module Main where

-- import Data.List
import Data.List.Split

import Debug.Trace
idtrace x = trace (show x) x

abba (a:r@(b:c:d:_)) | a /= b && a == d && b == c = True
                     | otherwise = abba r
abba _ = False

abbas [] = False
abbas [a] = abba a
abbas (a:b:r) = not (abba b) && (abba a || abbas r)

tls = abbas . splitOneOf "[]"

main = do
  cnt <- getContents
  print $ length $ filter tls $ lines cnt

