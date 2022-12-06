module Main where

import Data.List
import Data.List.Split

import Debug.Trace
idtrace x = trace (show x) x

type Room = (String, Int, String)

sectorid :: Room -> Int
sectorid (_, sid, _) = sid

parseRoom :: String -> Room
parseRoom s = (name, sid, checksum)
  where [fullname, checksum] = splitOn "[" $ init s
        splitname = splitOn "-" fullname
        name = foldl (++) "" $ init splitname
        sid = read $ last splitname

compareLength a b | length a == length b = compare (head a) (head b)
                  | otherwise = compare (length b) (length a)

real :: Room -> Bool
real (name, _, checksum) = checksum == calculate name
  where calculate name = idtrace $ take 5 $ map head $ sortBy compareLength $ group $ sort name

main = do
  cnt <- getContents
  print $ sum $ map sectorid $ filter real $ map parseRoom $ lines cnt

