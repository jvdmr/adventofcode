module Main where

-- import Data.List
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.Lazy as BL
import Data.Digest.Pure.MD5

correctHash key = "00000" == take 5 hashedKey
  where hashedKey = show $ md5 $ BLU.fromString key

mine n key | correctHash (key ++ show n) = n
           | otherwise = mine (n + 1) key

main = do
  cnt <- getContents
  print $ map (mine 1) $ lines cnt

