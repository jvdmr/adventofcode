module Main where

import Data.List hiding (empty, insert)
import Data.List.Split
import Data.Map (Map, empty, insert, toList)
import Text.Parsec
import Control.Monad.Identity (Identity)

import Debug.Trace
idtrace x = trace (show x) x
tagidtrace s x = trace (s ++ show x) x
ftrace f x = trace (f x) x

data Var = Var Char
         | Val Int
         deriving (Show, Eq)

data Instruction = Inp Var
                 | Add Var Var
                 | Mul Var Var
                 | Div Var Var
                 | Mod Var Var
                 | Eql Var Var
                 deriving (Show, Eq)

type Algorithm = [Instruction]

variable :: ParsecT String u Identity Var
variable =     (lower >>= return . Var)
           <|> (char '-' >> many1 digit >>= return . Val . negate . read)
           <|> (many1 digit >>= return . Val . read)
           <?> "variable"

instruction :: ParsecT String u Identity Instruction
instruction =     (string "inp" >> space >> variable >>= return . Inp)
              <|> (string "add" >> space >> variable >>= \a -> space >> variable >>= return . Add a)
              <|> (string "div" >> space >> variable >>= \a -> space >> variable >>= return . Div a)
              <|> (string "eql" >> space >> variable >>= \a -> space >> variable >>= return . Eql a)
              <|> try (string "mul" >> space >> variable >>= \a -> space >> variable >>= return . Mul a)
              <|> (string "mod" >> space >> variable >>= \a -> space >> variable >>= return . Mod a)
              <?> "instruction"

parseAlgorithm :: String -> Instruction
parseAlgorithm = right . parse (instruction) "(source)"
  where right (Right ast) = ast
        right (Left a) = error $ show a

resolvePart :: ([(Int, Int)], Map Int Int) -> (Int, Algorithm) -> ([(Int, Int)], Map Int Int)
resolvePart (stack, model) (i, part) = (stack', model')
  where (Div (Var 'z') (Val a)) = part !! 3
        (Add (Var 'x') (Val b)) = part !! 4
        (Add (Var 'y') (Val c)) = part !! 14
        stack' = if a == 1 then ((i, c):stack) else tail stack
        ((j, x):_) = stack
        diff = x + b
        (i', j', diff') = if diff < 0 then (j, i, -diff) else (i, j, diff)
        model' = if a == 1 then model else insert i' 9 $ insert j' (9 - diff') model

listToInt :: [Int] -> Int
listToInt [] = 0
listToInt is = (last is) + 10 * (listToInt $ init is)

findInput :: Algorithm -> Int
findInput algo = listToInt $ map snd $ toList $ snd $ foldl resolvePart ([], empty) $ zip [0..] $ ftrace (concat . map ((++"\n") . show)) $ tail $ splitOn [Inp (Var 'w')] algo

-- block:
-- -1 -- inp w       -- w = input                                                   -- -1 -- -- w is input
--  0 -- mul x 0     -- x = 0                                                       --  0 --     -- x is 0
--  1 -- add x z     -- x = z                                                       --  1 --     -- z is only 0 the first time, afterwards...
--  2 -- mod x 26    -- x = z % 26                                                  --  2 --
--  3 -- div z a     -- z = z div a (1 or 26)                                       --  3 --   -- a is 1 or 26, meaning z remains z or is divided wholly by 26
--  4 -- add x b     -- x = z % 26 + b                                              --  4 --   -- b is a number,   first time z is b
--  5 -- eql x w     -- x = 1 if z % 26 + b == w else 0                             --  5 --
--  6 -- eql x 0     -- x = 1 if z % 26 + b /= w else 0                             --  6 --       -- x is 0 if input equals z % 26 + b
--  7 -- mul y 0     -- y = 0                                                       --  7 --     
--  8 -- add y 25    -- y = 25                                                      --  8 --    -- y is 25
--  9 -- mul y x     -- y = 25 if z % 26 + b /= w else 0                            --  9 --
-- 10 -- add y 1     -- y = 26 if z % 26 + b /= w else 1                            -- 10 --
-- 11 -- mul z y     -- z = z div a (1 or 26) * y (26 or 1)                         -- 11 --
-- 12 -- mul y 0     -- y = 0                                                       -- 12 --
-- 13 -- add y w     -- y = w                                                       -- 13 --     --
-- 14 -- add y c     -- y = w + c                                                   -- 14 --   -- c is a number
-- 15 -- mul y x     -- y = (w + c) * x (1 or 0)                                    -- 15 --     --   if x is 0, c doesn't matter
-- 16 -- add z y     -- z = z div a (1 or 26) * y (26 or 1) + (w + c) * x (1 or 0)  -- 16 --     -- z is 0 at the end

main = do
  cnt <- getContents
  print $ findInput $ map parseAlgorithm $ lines cnt

