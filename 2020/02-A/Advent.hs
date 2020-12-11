module Main where

import Data.List

between a b c | a <= b = b <= c
              | otherwise = False

data Password = Password {min::Int, max::Int, char::Char, pw::String}
              deriving Show

countChar result c [] = result
countChar result c (s:rst) | c == s = countChar (result + 1) c rst
                           | otherwise = countChar result c rst

validPassword (Password min max char pw) = between min (countChar 0 char pw) max

parseRest car (' ':c:':':' ':cdr) (Password min _ _ _) = Password min (read car) c cdr
parseRest car (c:cdr) p = parseRest (car ++ [c]) cdr p

parseMinRange car ('-':cdr) (Password _ _ _ _) = parseRest "" cdr (Password (read car) undefined undefined undefined)
parseMinRange car (c:cdr) p = parseMinRange (car ++ [c]) cdr p

readData :: String -> Password
readData s = parseMinRange "" s (Password undefined undefined undefined undefined)

main = do
  cnt <- getContents
  print $ length $ filter validPassword $ map readData $ lines cnt

