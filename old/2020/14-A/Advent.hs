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

applyMask _ [] n = n
applyMask i ('X':rst) n = applyMask (i+1) rst n
applyMask i ('0':rst) n = applyMask (i+1) rst $ clearBit n i
applyMask i ('1':rst) n = applyMask (i+1) rst $ setBit n i

execute (mem, mask) (Mask val) = (mem, val)
execute (mem, mask) (Write addr val) = (insert addr (applyMask 0 mask val) mem, mask)

checkSum (mem, _) = M.foldl (+) 0 mem

main = do
  cnt <- getContents
  print $ checkSum $ L.foldl execute (empty, "") $ map parse $ lines cnt

