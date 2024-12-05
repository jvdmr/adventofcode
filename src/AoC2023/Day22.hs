{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC2023.Day22
  ( part1
  , part2
  ) where

import Data.List
import Data.List.Split (splitOn)
import Data.Ord (comparing)

import AoC (Solver)
import AoC.Util (strings, equating, last', andF)
import AoC.Grid (Grid, Coord, insertAt, (!), cget, Axis(..), fromCoords, mapG)
import AoC.Trace

type Range = (Coord Int, Coord Int)

expandRange :: Range -> [Coord Int]
expandRange ((x1, y1), (x2, y2)) = [(x, y) | x <- [min x1 x2..max x1 x2], y <- [min y1 y2..max y1 y2]]

type Name = String
type IBlock = (Name, ((Int, Int, Int), (Int, Int, Int)))
type IBlocks = [IBlock]

iblock :: Name -> String -> IBlock
iblock n s = (n, (a, b))
  where [a, b] = map (read . p) $ splitOn "~" s
        p str = "(" ++ str ++ ")"

iblocks :: [String] -> IBlocks
iblocks = zipWith iblock strings

data Block = Block { name :: Name, height :: Int, z :: Int, coords :: [Coord Int], supports :: [Block] }
           | Space
           deriving (Eq)

setSupports :: Block -> [Block] -> Block
setSupports (Block n h _ cs _) [] = Block n h 1 cs []
setSupports (Block n h _ cs _) s = Block n h nz cs s
  where nz = top $ head s

top :: Block -> Int
top block = z block + height block

block :: Block -> Bool
block Space = False
block _ = True

instance Ord Block where
  compare = comparing z

instance Show Block where
  show block@(Block _ _ _ _ _) = "(" ++ name block ++ ", " ++ show (height block) ++ ", " ++ show (z block) ++ ", " ++ show (head $ coords block) ++ "~" ++ show (last $ coords block) ++ ")"
  show space = "Space"

expand :: IBlock -> Block
expand (n, ((x1, y1, z1), (x2, y2, z2))) = Block n h lz (expandRange r') []
  where lz = min z1 z2
        h = 1 + (abs $ z1 - z2)
        r' = ((x1, y1), (x2, y2))

fall :: (Grid Block, [Block]) -> [Block] -> [Block]
fall (_, rbs) [] = reverse rbs
fall (g, rbs) (b:rst) = fall (g', b':rbs) rst
  where g' = insertAt g b' cs
        b' = setSupports b bs
        bs = last' $ groupBy (equating top) $ sortBy (comparing top) $ nub $ filter block $ map ((!) g) cs
        cs = coords b

settle :: [Block] -> [Block]
settle bs = fall (emptyGrid, []) $ sort bs
  where mx = maximum $ map (cget X) cs
        my = maximum $ map (cget Y) cs
        cs = concat $ map coords bs
        emptyGrid = mapG (\_ -> Space) $ fromCoords [(0, 0), (mx, my)]

loadbearing :: [Block] -> [Block]
loadbearing settled = nub $ concat $ filter ((== 1) . length) $ map supports settled

disintegratables :: [Block] -> [Block]
disintegratables settled = filter (flip notElem lb) settled
  where lb = loadbearing settled

part1 :: Solver
part1 = show . length . disintegratables . settle . map expand . iblocks . lines

part2 :: Solver
part2 = show . length . lines

