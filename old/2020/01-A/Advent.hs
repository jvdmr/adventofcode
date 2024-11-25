module Main where

-- import Data.List

checkSum [] _ _ = 0
checkSum _ [] _ = 0
checkSum (a:alst) (b:blst) fullst | a + b == 2020 = a * b
                                  | a + b < 2020 = checkSum (a:alst) blst fullst
                                  | otherwise = checkSum alst fullst fullst

findProduct lst = checkSum lst lst lst

main = do
  cnt <- getContents
  print $ findProduct $ sort $ map (read :: String -> Int) $ lines cnt

