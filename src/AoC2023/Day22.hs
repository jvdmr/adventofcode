{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC2023.Day22
  ( part1
  , part2
  ) where

import Data.List
import Data.List.Split (splitOn)
import Data.Ord (comparing)

import AoC (Solver)
import AoC.Util (strings, andF)
import AoC.Grid3D
import AoC.Trace

type Range = (Coord Int, Coord Int)

expandRange :: Range -> [Coord Int]
expandRange ((x1, y1, z1), (x2, y2, z2)) = [(x, y, z - 1) | x <- [min x1 x2..max x1 x2], y <- [min y1 y2..max y1 y2], z <- [min z1 z2..max z1 z2]]

type Name = String
type Block = (Name, Range)
type Blocks = [Block]

block :: Name -> String -> Block
block name s = (name, (a, b))
  where [a, b] = map (read . p) $ splitOn "~" s
        p str = "(" ++ str ++ ")"

blocks :: [String] -> Blocks
blocks = zipWith block strings

type ExpandedBlock = (Name, [Coord Int])
type ExpandedBlocks = [ExpandedBlock]

bname :: ExpandedBlock -> Name
bname = fst

bcoords :: ExpandedBlock -> [Coord Int]
bcoords = snd

expand :: Block -> ExpandedBlock
expand (name, r) = (name, expandRange r)

getz :: ExpandedBlock -> Int
getz (_, cs) = minimum $ map (cget Z) cs

sortEB :: ExpandedBlocks -> ExpandedBlocks
sortEB = sortBy $ comparing getz

dropBlock :: ExpandedBlock -> ExpandedBlock
dropBlock (n, c) = (n, map (go D) c)

fits :: Grid Name -> ExpandedBlock -> Bool
fits g (_, c) = all (flip andF [inGrid g, (== "") . (g !)]) c

fall :: (Grid Name, ExpandedBlocks) -> ExpandedBlocks -> (Grid Name, ExpandedBlocks)
-- fall (g, rbs) [] = (g, btrace $ reverse rbs)
fall (g, rbs) [] = (g, reverse rbs)
fall (g, rbs) (b:rst) | fits g b' = fall (g, rbs) (b':rst)
                      | otherwise = fall (insertAt g (bname b) (bcoords b), b:rbs) rst
                       where b' = dropBlock b

settle :: ExpandedBlocks -> (Grid Name, ExpandedBlocks)
settle bs = fall (emptyGrid, []) $ sortEB bs
  where mx = maximum $ map (cget X) cs
        my = maximum $ map (cget Y) cs
        mz = maximum $ map (cget Z) cs
        cs = concat $ map snd bs
        emptyGrid = mapG (\_ -> "") $ fromCoords [(0, 0, 0), (mx, my, mz)]

disintegratable :: Grid Name -> ExpandedBlocks -> ExpandedBlock -> Bool
-- disintegratable g bs b = (g'', bs') == rftrace show sf (fall (g', [])) bs'
disintegratable g bs b = (g'', bs') == fall (g', []) bs'
  where bs' = filter (/= b) bs
        g' = mapG (\_ -> "") g
        g'' = mapG noB g
        noB c | c == bname b = ""
              | otherwise = c
--         sf (sg, seb) = "(" ++ drawGrid (mapG head' sg) ++ ", " ++ show seb ++ ")"
--         head' l | length l == 0 = ' '
--                 | otherwise = head l

disintegratables :: ExpandedBlocks -> ExpandedBlocks
disintegratables bs = filter (disintegratable g settled) settled
  where (g, settled) = settle bs

showBlock :: ExpandedBlock -> String
showBlock (n, cs) = "(" ++ n ++ ", " ++ show (head cs) ++ "~" ++ show (last cs) ++ ")"

btrace :: ExpandedBlocks -> ExpandedBlocks
btrace = ftrace (intercalate "\n" . map showBlock)

part1 :: Solver
part1 = show . length . btrace . disintegratables . map expand . blocks . lines
-- part1 = show . length . disintegratables . map expand . blocks . lines

part2 :: Solver
part2 = show . length . lines

