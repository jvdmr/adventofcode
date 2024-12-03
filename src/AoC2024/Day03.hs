{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC2024.Day03
  ( part1
  , part2
  ) where

import AoC (Solver)
import AoC.Parsec

data Instruction = Mul Int Int
                 | Do
                 | Dont
                 | Ignored
                 deriving (Eq)

instance Show Instruction where
  show (Mul x y) = "mul(" ++ show x ++ "," ++ show y ++ ")"
  show Do = "do()"
  show Dont = "don't()"
  show Ignored = "*"

intPair :: Stream s m Char => ParsecT s u m (Int, Int)
intPair =     integer >>= \x -> char ',' >> integer >>= \y -> return (x, y)
          <?> "intPair"

mul :: Stream s m Char => ParsecT s u m Instruction
mul =     string "mul" >> parens intPair >>= (return . uncurry Mul)
      <?> "mul"

ignore :: Stream s m Char => ParsecT s u m Instruction
ignore =     anyChar >> return Ignored
         <?> "ignore"

instruction :: Stream s m Char => ParsecT s u m Instruction
instruction =     try mul
              <|> ignore
              <?> "instruction"

calc :: Instruction -> Int
calc (Mul x y) = x * y
calc _ = 0

part1 :: Solver
part1 = show . sum . map calc . right . parse (many instruction) "aoc input"

doins :: Stream s m Char => ParsecT s u m Instruction
doins =     string "do()" >> return Do
        <?> "do"

dont :: Stream s m Char => ParsecT s u m Instruction
dont =     string "don't()" >> return Dont
       <?> "dont"

instruction' :: Stream s m Char => ParsecT s u m Instruction
instruction' =     try mul
               <|> try doins
               <|> try dont
               <|> ignore
               <?> "instruction"

skipDont :: [Instruction] -> [Instruction]
skipDont [] = []
skipDont (Dont:rst) = skipDont $ dropWhile (/= Do) rst
skipDont (a:rst) = a:skipDont rst

part2 :: Solver
part2 = show . sum . map calc . skipDont . right . parse (many instruction') "aoc input"

