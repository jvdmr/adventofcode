module Main where

import Data.List as L hiding (insert)
import Data.List.Split
import Data.Map as M hiding (map)
import Data.Bits

data Instruction = Mask {val::String}
                 | Write {addr::Int, value::Int}

parseInstruction "mask" val = Mask $ reverse val
parseInstruction ('m':'e':'m':'[':rst) val = Write addr $ read val
  where addr = read $ init rst
parseInstruction s val = error s

parse line = parseInstruction cmd val
  where (cmd:val:[]) = splitOn " = " line

flatten = L.foldl (++) []

floatify i n = (setBit n i):(clearBit n i):[]

applyMask _ [] ns = ns
applyMask i ('X':rst) ns = applyMask (i+1) rst $ flatten $ map (floatify i) ns
applyMask i ('0':rst) ns = applyMask (i+1) rst ns
applyMask i ('1':rst) ns = applyMask (i+1) rst $ map (flip setBit $ i) ns

insertAtAddresses [] _ mem = mem
insertAtAddresses (addr:rst) val mem = insertAtAddresses rst val $ insert addr val mem

execute (mem, mask) (Mask val) = (mem, val)
execute (mem, mask) (Write addr val) = (insertAtAddresses (applyMask 0 mask [addr]) val mem, mask)

checkSum (mem, _) = M.foldl (+) 0 mem

main = do
  cnt <- getContents
  print $ checkSum $ L.foldl execute (empty, "") $ map parse $ lines cnt

