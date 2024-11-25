module Main where

-- import Data.List
import Data.List.Split (splitOn)
import Data.Map ((!))
import qualified Data.Map as M

import Debug.Trace
idtrace x = trace (show x) x

loop :: [a] -> [a]
loop a = a ++ loop a

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil f = takeWhile (not . f)

data Instruction = R | L
  deriving (Show, Eq)

readInstruction :: Char -> Instruction
readInstruction 'R' = R
readInstruction 'L' = L

type Instructions = [Instruction]
type Location = String
type MapNetwork = M.Map Location (Location, Location)
type MapState = (MapNetwork, Instructions, Location)

parseEdge :: String -> (Location, (Location, Location))
parseEdge s = (here, (left, right))
  where [here, rest] = splitOn " = (" $ init s
        [left, right] = splitOn ", " rest

parseMapNetwork :: [String] -> MapNetwork
parseMapNetwork = M.fromList . map parseEdge

createMap :: [String] -> MapState
createMap inputS = (network, loop $ map readInstruction $ head inputS, "AAA")
  where network = parseMapNetwork $ drop 2 inputS

go :: Instruction -> (Location, Location) -> Location
go L = fst
go R = snd

walk :: MapState -> MapState
walk (network, instruction:instructions, location) = (network, instructions, go instruction $ network ! location)

arrived :: MapState -> Bool
arrived (_, _, "ZZZ") = True
arrived _ = False

main = do
  cnt <- getContents
  print $ length $ takeUntil arrived $ iterate walk $ createMap $ lines cnt

