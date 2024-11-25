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

isA :: Location -> Bool
isA [_, _, 'A'] = True
isA _ = False

isZ :: Location -> Bool
isZ [_, _, 'Z'] = True
isZ _ = False

createMaps :: [String] -> [MapState]
createMaps inputS = [(network, instructions, location) | location <- filter isA $ M.keys network]
  where network = parseMapNetwork $ drop 2 inputS
        instructions = loop $ map readInstruction $ head inputS

go :: Instruction -> (Location, Location) -> Location
go L = fst
go R = snd

walk :: MapState -> MapState
walk (network, instruction:instructions, location) = (network, instructions, go instruction $ network ! location)

arrived :: MapState -> Bool
arrived (_, _, location) = isZ location

pathLength :: MapState -> Int
pathLength = length . takeUntil arrived . iterate walk

findLCM :: [Int] -> Int
findLCM = foldl lcm 1

main = do
  cnt <- getContents
  print $ findLCM $ map pathLength $ createMaps $ lines cnt

