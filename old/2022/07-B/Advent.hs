module Main where

import Data.List
import Data.List.Split

import Debug.Trace
idtrace x = trace (show x) x

data File = Dir String [File]
          | File String Int
          deriving (Eq, Show)

isDir :: File -> Bool
isDir (File _ _) = False
isDir (Dir _ _) = True

rootDir = Dir "/" []

newDir name = Dir name []

parseFile [size, name] = File name $ read size

parseFS :: File -> [[String]] -> (File, [[String]])
parseFS d [] = (d, [])
parseFS (Dir n _) (["$", "ls"]:nxt) = parseFS (Dir n fl) $ dropWhile ((/= "$") . head) nxt
  where fl = map parseFile $ filter ((/= "dir") . head) $ takeWhile ((/= "$") . head) nxt
parseFS d (["$", "cd", ".."]:nxt) = (d, nxt)
parseFS (Dir n fl) (["$", "cd", name]:nxt) = parseFS (Dir n (d:fl)) rnxt
  where (d, rnxt) = parseFS (newDir name) nxt

du :: File -> Int
du (File _ size) = size
du (Dir _ fl) = sum $ map du fl

allDirSizes :: [(Int, String)] -> [File] -> [(Int, String)]
allDirSizes r [] = r
allDirSizes r (d@(Dir name fl):rest) = allDirSizes ((du d, name):r) ((filter isDir fl) ++ rest)

smallestDirToDelete :: File -> (Int, String)
smallestDirToDelete d = head $ dropWhile ((< neededSpace) . fst) $ sort $ allDirSizes [] [d]
  where neededSpace = 30000000 - (70000000 - du d)

main = do
  cnt <- getContents
  print $ smallestDirToDelete $ fst $ parseFS rootDir $ map (splitOn " ") $ lines cnt

