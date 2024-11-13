module Main where

import Data.Char
import Data.List
import Data.List.Split

import Debug.Trace
idtrace x = trace (show x) x

type Room = (String, Int, String)

sectorid :: Room -> Int
sectorid (_, sid, _) = sid

join :: [a] -> [[a]] -> [a]
join j l = head l ++ foldl (++) [] (map (j++) $ tail l)

parseRoom :: String -> Room
parseRoom s = (name, sid, checksum)
  where [fullname, checksum] = splitOn "[" $ init s
        splitname = splitOn "-" fullname
        name = join "-" $ init splitname
        sid = read $ last splitname

compareLength a b | length a == length b = compare (head a) (head b)
                  | otherwise = compare (length b) (length a)

real :: Room -> Bool
real (name, _, checksum) = checksum == calculate name
  where calculate name = take 5 $ map head $ sortBy compareLength $ group $ sort $ filter (/= '-') name

rotateC :: Int -> Char -> Char
rotateC _ '-' = ' '
rotateC 0 c = c
rotateC n 'z' = rotateC (n - 1) 'a'
rotateC n c = rotateC (n - 1) (chr ((ord c) + 1))

decrypt :: Room -> Room
decrypt (s, sid, cs) = (decryptName s, sid, cs)
  where decryptName = map (rotateC (mod sid 26))

nameIs s (n, _, _) = s == n

main = do
  cnt <- getContents
  print $ head $ filter (nameIs "northpole object storage") $ map decrypt $ filter real $ map parseRoom $ lines cnt

