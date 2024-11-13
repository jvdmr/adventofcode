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

findPass s = take 8 $ map (head . drop 5 . idtrace) $ filter (/= "") $ map (mine s) [1..]

main = do
  cnt <- getContents
  print $ map findPass $ lines cnt

