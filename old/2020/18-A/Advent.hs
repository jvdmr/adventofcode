module Main where

import Debug.Trace
-- import Data.List
import Data.Char

data Token = Number Int
           | Parens [Token]
           | Other Char
           deriving (Eq)

instance Show Token where
  show (Number a) = show a
  show (Parens a) = show a
  show (Other a) = [a]

tokenize ast [] = (reverse ast, [])
tokenize ast (')':rst) = (reverse ast, rst)
tokenize ast ('(':rst) = tokenize ((Parens subast):ast) afterParens
  where (subast, afterParens) = tokenize [] rst
tokenize ast (a:rst) | isDigit a = tokenize ((Number $ read [a]):ast) rst
                     | otherwise = tokenize ((Other a):ast) rst

skipParens = tail . dropWhile (/= Other ')')

fakeMath :: String -> Int -> Char -> [Token] -> Int
fakeMath pref result _ [] = result
fakeMath pref result op (Parens ast:rst) = fakeMathTrace pref result op $ (Number $ fakeMathTrace ("> " ++ pref) 0 'x' ast):rst
fakeMath pref result _ (Other a:rst) = fakeMathTrace pref result a rst
fakeMath pref result '+' (Number a:rst) = fakeMathTrace pref (result + a) 'x' rst
fakeMath pref result '*' (Number a:rst) = fakeMathTrace pref (result * a) 'x' rst
fakeMath pref _ 'x' (Number a:rst) = fakeMathTrace pref a 'x' rst

fakeMathTrace :: String -> Int -> Char -> [Token] -> Int
fakeMathTrace pref result op rst = trace (pref ++ show result ++ " - " ++ show op ++ " - " ++ show rst) $ fakeMath pref result op rst

nospaces [] = []
nospaces (' ':rst) = nospaces rst
nospaces (a:rst) = a:nospaces rst

main = do
  cnt <- getContents
  print $ map (fakeMathTrace "" 0 'x' . fst . tokenize [] . nospaces) $ lines cnt
  print $ foldl (+) 0 $ map (fakeMathTrace "" 0 'x' . fst . tokenize [] . nospaces) $ lines cnt

