module Main where

import Data.List

import Debug.Trace
idtrace x = trace (show x) x

snafuToInt :: String -> Int
snafuToInt "=" = -2
snafuToInt "-" = -1
snafuToInt n@[_] = read n
snafuToInt n = 5 * snafuToInt (init n) + snafuToInt [last n]

intToSnafu :: Int -> String
intToSnafu (-2) = "="
intToSnafu (-1) = "-"
intToSnafu n | n <= 2 = show n
             | n <= 4 = intToSnafu (n - 5)
             | otherwise = intToSnafu (div (n + r) 5) ++ intToSnafu (mod n 5)
             where r | mod n 5 > 2 = 5
                     | otherwise = 0

main = do
  cnt <- getContents
  print $ intToSnafu $ sum $ map snafuToInt $ lines cnt

