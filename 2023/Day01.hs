{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module Day01
  ( part1
  , part2
  ) where

import Data.List
import Data.Char

import Vdmr.Generic

fldigits :: [Char] -> Int
fldigits l = read $ [head ds, last ds]
  where ds = filter isDigit l

digitize :: [Char] -> [Char]
digitize [] = []
digitize ('o':rest@('n':'e':_)) = '1':digitize rest
digitize ('t':rest@('w':'o':_)) = '2':digitize rest
digitize ('t':rest@('h':'r':'e':'e':_)) = '3':digitize rest
digitize ('f':rest@('o':'u':'r':_)) = '4':digitize rest
digitize ('f':rest@('i':'v':'e':_)) = '5':digitize rest
digitize ('s':rest@('i':'x':_)) = '6':digitize rest
digitize ('s':rest@('e':'v':'e':'n':_)) = '7':digitize rest
digitize ('e':rest@('i':'g':'h':'t':_)) = '8':digitize rest
digitize ('n':rest@('i':'n':'e':_)) = '9':digitize rest
digitize (d:rest) = d:digitize rest

part1 :: Solver
part1 = show . sum . map fldigits . lines

part2 :: Solver
part2 = show . sum . map (fldigits . digitize) . lines

