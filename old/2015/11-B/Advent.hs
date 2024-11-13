module Main where

import Data.Char
import Data.List hiding (insert)

import Debug.Trace
idtrace x = trace (show x) x

straight :: String -> Bool
straight (_:_:[]) = False
straight (a:b:c:rst) | 1 + ord a == ord b && 1 + ord b == ord c = True
                     | otherwise = straight (b:c:rst)

repeats :: Int -> String -> Bool
repeats 2 _ = True
repeats _ [] = False
repeats _ (_:[]) = False
repeats n (a:b:rst) | a == b = repeats (n + 1) rst
                    | otherwise = repeats n (b:rst)

validPass :: String -> Bool
validPass pass = straight pass && repeats 0 pass

invalidPass :: String -> Bool
invalidPass pass | elem 'i' pass = True
                 | elem 'o' pass = True
                 | elem 'l' pass = True
                 | otherwise = not $ validPass pass

incPass :: String -> String
incPass [] = "a"
incPass ('z':rst) = 'a':incPass rst
incPass ('h':rst) = 'j':rst
incPass ('k':rst) = 'm':rst
incPass ('n':rst) = 'p':rst
incPass (c:rst) = (chr (1 + ord c)):rst

nextPass :: String -> String
nextPass pass = head $ dropWhile invalidPass $ drop 1 $ map reverse $ iterate incPass $ reverse pass

main = do
  cnt <- getContents
  print $ map nextPass $ map nextPass $ lines cnt

