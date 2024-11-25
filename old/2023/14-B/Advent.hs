module Main where

-- import Data.List
import Data.List.Split

import Debug.Trace
idtrace x = trace (show x) x

ftrace :: (a -> String) -> a -> a
ftrace f x = trace (f x) x

showGrid g = concat $ map ((++ "\n") . show) g

rotateCW :: [[a]] -> [[a]]
rotateCW = transpose . reverse

east :: [String] -> [String]
east = map $ concat . map sort . split (keepDelimsL $ oneOf "#")

loadNorth :: [String] -> Int
loadNorth = sum . zipWith (*) [1..] . reverse . map (length . filter (== 'O'))

takeUniq :: (Eq a) => [a] -> [a]
takeUniq = takeUniq' []
  where takeUniq' prev (a:rest) | a `elem` prev = reverse prev
                                | otherwise = takeUniq' (a:prev) rest

findRepeatCycle :: (Eq a) => [a] -> (Int, Int)
findRepeatCycle lst = (cs, cl)
  where rs = takeUniq lst
        r = lst !! length rs
        cs = length $ takeWhile (/= r) rs
        cl = length rs - cs

dropCyclesAndGet :: (Eq a) => Int -> [a] -> a
dropCyclesAndGet i lst = lst !! i'
  where (start, l) = findRepeatCycle lst
        i'  | i < start + l = i
            | otherwise = start + (i - start) `mod` l

nextDir = east . rotateCW
cycleP = nextDir . nextDir . nextDir . nextDir

main = do
  cnt <- getContents
  print $ loadNorth $ dropCyclesAndGet 1000000000 $ iterate cycleP $ lines cnt

