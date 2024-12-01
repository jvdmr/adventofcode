{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC2023.Day22
  ( part1
  , part2
  ) where

import Data.List
import Data.List.Split (splitOn)
-- import Data.Map ((!))
import qualified Data.Map as M

import AoC (Solver)
import AoC.Util (strings, iterateUntilIdempotent)
import AoC.Grid3D

type Range = (Coord Int, Coord Int)

expandRange :: Range -> [Coord Int]
expandRange ((x1, y1, z1), (x2, y2, z2)) = [(x, y, z) | x <- [min x1 x2..max x1 x2], y <- [min y1 y2..max y1 y2], z <- [min z1 z2..max z1 z2]]

type Name = String
type Block = (Name, Range)
type Blocks = [Block]

block :: Name -> String -> Block
block name s = (name, (read a, read b))
  where [a, b] = map p $ splitOn "~" s
        p str = "(" ++ str ++ ")"

blocks :: [Name] -> Blocks
blocks = zipWith block strings

type ExpandedBlock = (Name, [Coord Int])
type ExpandedBlocks = [ExpandedBlock]

bname :: ExpandedBlock -> Name
bname (name, _) = name

expand :: Block -> ExpandedBlock
expand (name, r) = (name, expandRange r)

sortEB :: ExpandedBlocks -> ExpandedBlocks
sortEB = sortBy compareZ
  where getz (_, cs) = minimum $ map (cget Z) cs
        compareZ a b = compare (getz a) (getz b)

dropBlock :: ExpandedBlock -> ExpandedBlock
dropBlock (n, c) = (n, map (go D) c)

fromBlock :: ExpandedBlock -> Grid Name
fromBlock (name, bcs) = drawCoords name "" bcs

tooLow :: ExpandedBlock -> Bool
tooLow (_, c) = any ((< 0) . cget Z) c

fits :: Grid Name -> ExpandedBlock -> Bool
fits g b@(n, c) | tooLow b = False
                | otherwise = all (flip elem ["", n] . (g !)) c

(!?) :: M.Map (Coord Int) Name -> Coord Int -> Name
(!?) m c | M.member c m = m M.! c
         | otherwise = ""

fromBlocks :: ExpandedBlocks -> Grid Name
fromBlocks bs = mapG (cns !?) $ fromCoords [(0, 0, 0), (mx, my, mz)]
  where mx = maximum $ map (cget X) cs
        my = maximum $ map (cget Y) cs
        mz = maximum $ map (cget Z) cs
        cs = concat $ map snd bs
        cns = M.fromList [(c, n) | (n, bcs) <- bs, c <- bcs]

fall :: ExpandedBlocks -> ExpandedBlocks
fall bs | [] == dropped = bs
        | otherwise = M.toList $ uncurry M.insert (head dropped) bmap
  where bmap = M.fromList bs
        g = fromBlocks bs
        sbs = sortEB bs
        dropped = filter (fits g) $ map dropBlock sbs

disintegratable :: ExpandedBlocks -> Name -> Bool
disintegratable bs name = bs' == fall bs'
  where bs' = filter ((/= name) . bname) bs

disintegratables :: ExpandedBlocks -> ExpandedBlocks
disintegratables bs = filter (disintegratable settled . bname) settled
  where settled = last $ iterateUntilIdempotent fall bs

part1 :: Solver
part1 = show . length . disintegratables . map expand . blocks . lines

part2 :: Solver
part2 = show . length . lines

