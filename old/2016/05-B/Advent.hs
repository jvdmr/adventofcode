module Main where

import Data.List
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.Lazy as BL
import Data.Digest.Pure.MD5

import Debug.Trace
idtrace x = trace (show x) x

correctHash key | "00000" == take 5 hashedKey = hashedKey
                | otherwise = ""
  where hashedKey = show $ md5 $ BLU.fromString key

mine key n = correctHash (key ++ show n)

takeFirst :: Eq b => (a -> b) -> [b] -> [a] -> [a]
takeFirst f found (l:ls) | elem (f l) found = takeFirst f found ls
                         | otherwise = l:takeFirst f ((f l):found) ls
findPass s = map (head . tail) $ sort $ take 8 $ map idtrace $ takeFirst head [] $ filter ((flip elem ['0'..'7']) . head) $ map (take 2 . drop 5) $ filter (/= "") $ map (mine s) [1..]

main = do
  cnt <- getContents
  print $ map findPass $ lines cnt

