module Main where

-- import Data.List

data Instruction = Nop {value::Int}
                 | Acc {value::Int}
                 | Jmp {target::Int}
                 | Done
                 deriving Show

getValue ('+':value) = read value
getValue ('-':value) = -(read value)

parseInstruction ('n':'o':'p':' ':value) = Nop $ getValue value
parseInstruction ('a':'c':'c':' ':value) = Acc $ getValue value
parseInstruction ('j':'m':'p':' ':value) = Jmp $ getValue value
parseInstruction bla = error bla

jumpTo program address = head $ drop address program

markExecuted program address = (take address program) ++ [Done] ++ (tail $ drop address program)

execute _ accumulator _ Done = accumulator
execute program accumulator address (Acc value) = execute newProgram (accumulator + value) (address + 1) $ jumpTo newProgram (address + 1)
  where newProgram = markExecuted program address
execute program accumulator address (Jmp target) = execute newProgram accumulator (address + target) $ jumpTo newProgram (address + target)
  where newProgram = markExecuted program address
execute program accumulator address (Nop _) = execute newProgram accumulator (address + 1) $ jumpTo newProgram (address + 1)
  where newProgram = markExecuted program address

main = do
  cnt <- getContents
  print $ (\program -> execute program 0 0 (Nop 0)) $ map parseInstruction $ lines cnt

