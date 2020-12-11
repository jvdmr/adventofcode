module Main where

import Data.List

checkSum [] _ _ _ = 0
checkSum _ [] _ _ = 0
checkSum _ _ [] _ = 0
checkSum (a:alst) (b:blst) (c:clst) fullst | a + b + c == 2020 = a * b * c
                                           | a + b + c < 2020 = checkSum (a:alst) (b:blst) clst fullst
                                           | a + b < 2020 = checkSum (a:alst) blst fullst fullst
                                           | otherwise = checkSum alst fullst fullst fullst

findProduct lst = checkSum lst lst lst lst

main = do
  cnt <- getContents
  print $ findProduct $ sort $ map (read :: String -> Int) $ lines cnt

