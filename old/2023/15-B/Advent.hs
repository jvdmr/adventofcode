module Main where

-- import Data.List
import Data.List.Split
import Data.Char
import Data.Map ((!))
import qualified Data.Map as M

import Debug.Trace
idtrace x = trace (show x) x

hash :: Int -> [Char] -> Int
hash i [] = i
hash i (c:rest) = hash i' rest
  where i' = ((i + ord c) * 17) `mod` 256

type Place = Int
type Lens = (String, Int)
type Box = [Lens]
type Boxes = M.Map Place Box
type Operation = Char

filterSet :: (a -> Bool) -> [a] -> [a] -- filter element out of a list when we are sure this element only occurs 0 or 1 times
filterSet _ [] = []
filterSet f (a:as) | not $ f a = as
                   | otherwise = a:filterSet f as

fixLens :: Box -> Lens -> Box
fixLens [] lens = [lens]
fixLens (other@(l, _):rest) lens@(label, _) | l == label = lens:rest
                                            | otherwise = other:fixLens rest lens

fix :: Box -> Operation -> Lens -> Box
fix box '-' (label, _) = filterSet ((/= label) . fst) box
fix box '=' lens = fixLens box lens

emptyBoxes :: Boxes
emptyBoxes = M.fromList $ zipWith (,) [0..255] $ repeat []

step :: Boxes -> String -> Boxes
step boxes s = M.insert box contents boxes
  where [label, ops, fl] = split (oneOf "=-") s
        box = hash 0 label
        op = head ops
        contents = fix (boxes ! box) op (label, read fl)

power :: Boxes -> Int
power boxes = sum $ map boxpower $ M.toList boxes
  where boxpower (place, box) = sum $ map ((place + 1) *) $ zipWith (*) [1..] $ map snd box

main = do
  cnt <- getContents
  print $ power $ foldl step emptyBoxes $ splitOn "," $ head $ lines cnt

