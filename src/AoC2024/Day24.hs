{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC2024.Day24
  ( part1
  , part2
  , tests
  ) where

import Prelude hiding (foldl) -- use foldl' from Data.List if you need foldl
import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Map ((!))
import qualified Data.Map as M

import AoC (Solver, Tests)
import AoC.Util (binToInt, boolToBin)

type Scope = M.Map String Bool

initvar :: String -> (String, Bool)
initvar s = (name, value == "1")
  where [name, value] = splitOn ": " s

initscope :: [String] -> Scope
initscope = M.fromList . map initvar

exec :: String -> Bool -> Bool -> Bool
exec "AND" = (&&)
exec "OR" = (||)
exec "XOR" = (/=)

makeGate :: Scope -> String -> Bool
makeGate s gs = exec op (s ! a) (s ! b)
  where [a, op, b] = splitOn " " gs

scope :: [[String]] -> Scope
scope [initvars, gates] = s
  where is = initscope initvars
        s = foldl' gate is gates
        gate m g = let [gs, k] = splitOn " -> " g in M.insert k (makeGate s gs) m

znum :: Scope -> Int
znum s = binToInt $ boolToBin $ map ((!) s) $ reverse $ sort $ filter ((==) 'z' . head) $ M.keys s

tests :: Tests
tests =
  [ show . length . lines
  ]

part1 :: Solver
part1 = show . znum . scope . splitOn [""] . lines

part2 :: Solver
part2 = ("Not yet solved! " ++) . show . length . lines

