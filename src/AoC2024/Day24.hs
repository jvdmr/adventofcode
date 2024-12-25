{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC2024.Day24
  ( part1
  , part2
  , tests
  ) where

import Prelude hiding (foldl) -- use foldl' from Data.List if you need foldl
import Data.List (sort, intercalate)
import Data.List.Split (splitOn)
import Data.Map ((!))
import qualified Data.Map as M

import AoC (Solver, Tests)
import AoC.Util (binToInt, boolToBin)
import AoC.Trace

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

part1 :: Solver
part1 = show . znum . scope . splitOn [""] . lines

data Instruction = And String [String]
                 | Or String [String]
                 | Xor String [String]
                 | Var String Bool
  deriving (Eq, Show)

name :: Instruction -> String
name (And n _) = n
name (Or n _) = n
name (Xor n _) = n
name (Var n _) = n

type InertScope = M.Map String Instruction

inertvar :: String -> (String, Instruction)
inertvar s = (name, Var name $ value == "1")
  where [name, value] = splitOn ": " s

initinertscope :: [String] -> InertScope
initinertscope = M.fromList . map inertvar

inert :: String -> String -> [String] -> Instruction
inert "AND" = And
inert "OR" = Or
inert "XOR" = Xor

inertGate :: String -> String -> Instruction
inertGate gs n = inert op n [a, b]
  where [a, op, b] = splitOn " " gs

inertscope :: [[String]] -> InertScope
inertscope [initvars, gates] = s
  where is = initinertscope initvars
        s = foldl' gate is gates
        gate m g = let [gs, k] = splitOn " -> " g in M.insert k (inertGate gs k) m

tests :: Tests
tests =
  [ show . length . lines
  , intercalate "\n" . map show . M.toList . inertscope . splitOn [""] . lines
  ]

-- 5-gate full adder tracing

matchRules :: [[String]] -> Instruction -> Bool
matchRules [xors, ands, ors] (Or "z45" _) = True
matchRules [xors, ands, ors] (Xor ('z':_) _) = True
matchRules [xors, ands, ors] (And ('z':_) _) = False
matchRules [xors, ands, ors] (Or n ps) = elem n xors
matchRules [xors, ands, ors] (Xor n ps) | all (flip elem "xy" . head) ps = elem n xors
                                        | otherwise = False
matchRules [xors, ands, ors] (And n ps) | all ((==) "00" . tail) ps = elem n xors
                                        | otherwise = elem n ors
matchRules [xors, ands, ors] _ = True

sortTargets :: [[String]] -> Instruction -> [[String]]
sortTargets [xors, ands, ors] (Xor _ ps) = [ps ++ xors, ands, ors]
sortTargets [xors, ands, ors] (And _ ps) = [xors, ps ++ ands, ors]
sortTargets [xors, ands, ors] (Or _ ps) = [xors, ands, ps ++ ors]
sortTargets lsts _ = lsts

swapped :: InertScope -> String
swapped s = intercalate "," $ sort $ wrong
  where wrong = map name $ ftrace (intercalate "\n" . map show) $ filter (not . matchRules lsts) $ M.elems s
        lsts = foldl' sortTargets [[], [], []] $ M.elems s

part2 :: Solver
part2 = swapped . inertscope . splitOn [""] . lines

