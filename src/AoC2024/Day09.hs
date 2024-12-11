{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC2024.Day09
  ( part1
  , part2
  , tests
  ) where

import Data.Ord (comparing)

import AoC (Solver, Tests)

data DiskData = File { fid :: Int, dl :: Int }
              | Space { fid :: Int, dl :: Int }
  deriving (Eq, Show)

instance Ord DiskData where
  compare = comparing dl

space :: Int -> DiskData
space l = Space 0 l

isFile :: DiskData -> Bool
isFile (File _ _) = True
isFile _ = False

isSpace :: DiskData -> Bool
isSpace (Space _ _) = True
isSpace _ = False

disk :: Int -> [Int] -> [DiskData]
disk _ [] = []
disk i [n] = [File i n]
disk i (fn:sn:rst) = File i fn:space sn:disk (i + 1) rst

defrag :: [DiskData] -> [DiskData]
defrag [] = []
defrag [Space _ _] = []
defrag ((Space _ 0):ds) = defrag ds
defrag ((Space _ l):ds) | isSpace (last ds) = defrag ((space l):init ds)
                        | l >= fl = (File i fl):defrag ((space (l - fl)):init ds)
                        | otherwise = (File i l):defrag (init ds ++ [File i (fl - l)])
  where (File i fl) = last ds
defrag (d:ds) = d:defrag ds

expand :: DiskData -> [Int]
expand f = take (dl f) $ repeat (fid f)

checksum :: [DiskData] -> Int
checksum = sum . zipWith (*) [0..] . concat . map expand

tests :: Tests
tests = [show . disk 0 . map (read . (:[])) . head . lines]

part1 :: Solver
part1 = show . checksum . defrag . disk 0 . map (read . (:[])) . head . lines

freeSpace :: DiskData -> DiskData -> DiskData
freeSpace free@(File _ fl) = freeSpace'
  where freespace' s@(Space _ _) = s
        freeSpace' f | free == f = space fl
                     | otherwise = f

defrag' :: [DiskData] -> [DiskData]
defrag' [] = []
defrag' [Space _ _] = []
defrag' ((Space _ 0):ds) = defrag' ds
defrag' ((Space _ l):ds) | isSpace (last ds) = defrag' ((space l):init ds)
                         | length matching == 0 = (space l):defrag' ds
                         | otherwise = f:defrag' ((space (l - fl)):map (freeSpace f) ds)
  where f@(File i fl) = head matching
        matching = filter ((l >=) . dl) $ filter isFile $ reverse ds
defrag' (d:ds) = d:defrag' ds

part2 :: Solver
part2 = show . checksum . defrag' . disk 0 . map (read . (:[])) . head . lines

