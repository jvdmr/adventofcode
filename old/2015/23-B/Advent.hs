module Main where

-- import Data.List

import Debug.Trace
idtrace x = trace (show x) x

applyN f n a = iterate f a !! n

type Register = Char
-- registers (a, b)
type Registry = (Int, Int)

data Instruction = HLF Register
                 | TPL Register
                 | INC Register
                 | JMP Int
                 | JIE Register Int
                 | JIO Register Int
                 | END
                 deriving (Eq, Show)

readOffset :: String -> Int
readOffset ('+':n) = read n
readOffset n = read n

readInstruction :: String -> Instruction
readInstruction ('h':'l':'f':' ':r:[]) = HLF r
readInstruction ('t':'p':'l':' ':r:[]) = TPL r
readInstruction ('i':'n':'c':' ':r:[]) = INC r
readInstruction ('j':'m':'p':' ':offset) = JMP $ readOffset offset
readInstruction ('j':'i':'e':' ':r:',':' ':offset) = JIE r $ readOffset offset
readInstruction ('j':'i':'o':' ':r:',':' ':offset) = JIO r $ readOffset offset

type Program = [Instruction]

nxt :: Program -> Program
nxt (i:is) = is ++ [i]

prv :: Program -> Program
prv (is) = last is:init is

hlf :: Registry -> Register -> Registry
hlf (a, b) 'a' = (div a 2, b)
hlf (a, b) 'b' = (a, div b 2)

tpl :: Registry -> Register -> Registry
tpl (a, b) 'a' = (a * 3, b)
tpl (a, b) 'b' = (a, b * 3)

inc :: Registry -> Register -> Registry
inc (a, b) 'a' = (a + 1, b)
inc (a, b) 'b' = (a, b + 1)

jmp :: Program -> Int -> Program
jmp p i | i >= 0 = applyN nxt i p
        | otherwise = applyN prv (- i) p

noop :: Program -> a -> Program
noop p _ = nxt p

jie :: Registry -> Register -> Program -> Int -> Program
jie (a, _) 'a' | even a = jmp
               | otherwise = noop
jie (_, b) 'b' | even b = jmp
               | otherwise = noop

jio :: Registry -> Register -> Program -> Int -> Program
jio (a, _) 'a' | a == 1 = jmp
               | otherwise = noop
jio (_, b) 'b' | b == 1 = jmp
               | otherwise = noop

execute :: Registry -> Program -> Registry
execute rs (END:_) = rs
execute rs p@(HLF r:_) = execute (hlf rs r) $ nxt p
execute rs p@(TPL r:_) = execute (tpl rs r) $ nxt p
execute rs p@(INC r:_) = execute (inc rs r) $ nxt p
execute rs p@(JMP i:_) = execute rs $ jmp p i
execute rs p@(JIE r i:_) = execute rs $ jie rs r p i
execute rs p@(JIO r i:_) = execute rs $ jio rs r p i

end :: Program -> Program
end p = p ++ [END]

main = do
  cnt <- getContents
  print $ execute (1, 0) $ end $ map readInstruction $ lines cnt

