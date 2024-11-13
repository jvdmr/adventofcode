module Main where

import Debug.Trace

import Data.List

import Text.Parsec

data Instruction = Fwd Int
                 | Down Int
                 | Up Int
                 deriving Show

instr =     try (string "forward" >> return Fwd)
        <|> try (string "down" >> return Down)
        <|> (string "up" >> return Up)
        <?> "instr"

expr =     instr >>= \i -> space >> digit >>= return . i . read . (:[])
       <?> "expr"

tokenize = right . parse expr "(source)"
  where right (Right ast) = ast

navigate (a, f, d) (Fwd n) = (a, f + n, d + (a * n))
navigate (a, f, d) (Down n) = (a + n, f, d)
navigate (a, f, d) (Up n) = (a - n, f, d)

checksum (_, a, b) = a * b

main = do
  cnt <- getContents
  print $ checksum $ foldl navigate (0, 0, 0) $ map tokenize $ lines cnt

