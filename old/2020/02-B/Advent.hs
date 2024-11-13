module Main where

import Data.List

data Password = Password {min::Int, max::Int, char::Char, pw::String}
              deriving Show

xor a b = (a || b) && (not (a && b))

charAtPos char pos s = char == (head $ drop (pos - 1) s)

validPassword (Password min max char pw) = charAtPos char min pw `xor` charAtPos char max pw

parseRest car (' ':c:':':' ':cdr) (Password min _ _ _) = Password min (read car) c cdr
parseRest car (c:cdr) p = parseRest (car ++ [c]) cdr p

parseMinRange car ('-':cdr) (Password _ _ _ _) = parseRest "" cdr (Password (read car) undefined undefined undefined)
parseMinRange car (c:cdr) p = parseMinRange (car ++ [c]) cdr p

readData :: String -> Password
readData s = parseMinRange "" s (Password undefined undefined undefined undefined)

main = do
  cnt <- getContents
  print $ length $ filter validPassword $ map readData $ lines cnt

