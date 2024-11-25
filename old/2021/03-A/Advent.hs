module Main where

-- import Data.List

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

binToDec :: Integral i => i -> i
binToDec 0 = 0
binToDec i = 2 * binToDec (div i 10) + (mod i 10)

main = do
  cnt <- getContents
  let gamma = map pairToBin $ zipAllWith (+) $ map (map binToPair) $ lines cnt
  print $ (binToDec $ read gamma) * (binToDec $ read $ map gammaToEpsilon gamma)

