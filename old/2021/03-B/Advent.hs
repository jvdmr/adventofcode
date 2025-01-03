module Main where

import Debug.Trace
-- import Data.List

idtrace a = trace (show a) a

binToPair '0' = (1, 0)
binToPair '1' = (0, 1)

instance (Num a, Num b) => Num (a, b) where
  (a, b) + (x, y) = (a + x, b + y)

zipAllWith :: (a -> a -> a) -> [[a]] -> [a]
zipAllWith f ([]:_) = []
zipAllWith f as = (foldl f (head (head as)) (map head (tail as))):(zipAllWith f $ map tail as)

pairToBin (a, b) | a > b = '0'
                 | otherwise = '1'

gammaToEpsilon '0' = '1'
gammaToEpsilon '1' = '0'

filterNumbers _ [a] = a
filterNumbers f as = n:(filterNumbers f $ map tail $ filter ((n ==) . head) as)
  where n = head $ f as

oxygen ns = filterNumbers gamma ns
  where gamma = map pairToBin . zipAllWith (+) . map (map binToPair)

co2 ns = filterNumbers epsilon ns
  where epsilon = map (gammaToEpsilon . pairToBin) . zipAllWith (+) . map (map binToPair)

binToDec :: Integral i => i -> i
binToDec 0 = 0
binToDec i = 2 * binToDec (div i 10) + (mod i 10)

main = do
  cnt <- getContents
  print $ foldl (*) 1 $ idtrace $ map (binToDec . read . ($ lines cnt)) [oxygen, co2]

