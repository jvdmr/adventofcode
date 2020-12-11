module Main where

import Data.List
import Text.PrettyPrint

data Instruction = Nop {value::Int}
                 | Acc {value::Int}
                 | Jmp {target::Int}
                 | Start
                 | Done
                 | End
                 deriving Show

getValue ('+':value) = read value
getValue ('-':value) = -(read value)

parseInstruction ('n':'o':'p':' ':value) = Nop $ getValue value
parseInstruction ('a':'c':'c':' ':value) = Acc $ getValue value
parseInstruction ('j':'m':'p':' ':value) = Jmp $ getValue value
parseInstruction bla = error bla

jumpTo program address = head $ drop address program

markExecuted program (-1) = program
markExecuted program address = (take address program) ++ [Done] ++ (tail $ drop address program)

execute _ accumulator address Done = (address, Done, accumulator):[]
execute _ accumulator address End = (address, End, accumulator):[]
execute program accumulator address Start = execute program accumulator address $ jumpTo program address
execute program accumulator address (Acc value) = (address, Acc value, accumulator):(execute newProgram (accumulator + value) (address + 1) $ jumpTo newProgram (address + 1))
  where newProgram = markExecuted program address
execute program accumulator address (Jmp target) = (address, Jmp target, accumulator):(execute newProgram accumulator (address + target) $ jumpTo newProgram (address + target))
  where newProgram = markExecuted program address
execute program accumulator address (Nop value) = (address, Nop value, accumulator):(execute newProgram accumulator (address + 1) $ jumpTo newProgram (address + 1))
  where newProgram = markExecuted program address

switch ((Nop value):rest) = (Jmp value):rest
switch ((Jmp value):rest) = (Nop value):rest

permutate prev [] = prev:[]
permutate prev program@(ins@(Nop value):rest) = (prev ++ switch program):(permutate (prev ++ [ins]) rest)
permutate prev program@(ins@(Jmp value):rest) = (prev ++ switch program):(permutate (prev ++ [ins]) rest)
permutate prev (ins:rest) = permutate (prev ++ [ins]) rest

prettyExec [] = empty
prettyExec ((address, ins, accumulator):rst) = int address <+> text (show ins) <+> int accumulator $$ prettyExec rst

pretty [] = empty
pretty ((program, execution):rst) = prettyExec execution $$ pretty rst

ended (_, End, _) = True
ended (_, _, _) = False

main = do
  cnt <- getContents
  print $ pretty $ filter (ended . last . snd) $ map (\program -> (program, execute program 0 0 Start)) $ permutate [] $ (++ [End]) $ map parseInstruction $ lines cnt
