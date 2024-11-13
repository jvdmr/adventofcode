module Main where

import Data.Bits
import Data.List hiding (insert)
import Data.List.Split
import Data.Map (insert, (!))
import qualified Data.Map as M

import Text.Parsec

import Debug.Trace
idtrace x = trace (show x) x
resulttrace ins x = trace (show ins ++ " -> " ++ show x) x

type Circuit = M.Map String Expr

data Wire = Wire String Expr
          deriving (Eq)

instance Show Wire where
  show (Wire name e) = show e ++ " -> " ++ show name

data Expr = ANum Int
          | AVar String
          | AAndB Expr Expr
          | AOrB Expr Expr
          | NotA Expr
          | ALShiftB Expr Int
          | ARShiftB Expr Int
          deriving (Eq)

instance Show Expr where
  show (ANum a) = show a
  show (AVar a) = show a
  show (AAndB a b) = show a ++ " AND " ++ show b
  show (AOrB a b) = show a ++ " OR " ++ show b
  show (NotA a) = "NOT " ++ show a
  show (ALShiftB a b) = show a ++ " LSHIFT " ++ show b
  show (ARShiftB a b) = show a ++ " RSHIFT " ++ show b

term =     try (many1 letter >>= return . AVar)
       <|> (many1 digit >>= return . ANum . read)
       <?> "term"

expr =     try (string "NOT " >> expr >>= return . NotA)
       <|> try (term >>= \a -> string " AND " >> expr >>= return . AAndB a)
       <|> try (term >>= \a -> string " OR " >> expr >>= return . AOrB a)
       <|> try (term >>= \a -> string " LSHIFT " >> many1 digit >>= return . ALShiftB a . read)
       <|> try (term >>= \a -> string " RSHIFT " >> many1 digit >>= return . ARShiftB a . read)
       <|> term
       <?> "expr"

wire =     (expr >>= \a -> string " -> " >> many1 letter >>= return . flip Wire a)
       <?> "wire"

evalExpr :: String -> Wire
evalExpr = right . parse wire "(source)"
  where right (Right ast) = ast

signal :: Int -> Int
signal s = mod s $ bit 16

execExpr :: Circuit -> Expr -> (Circuit, Int)
execExpr circuit (ANum a) = (circuit, signal a)
execExpr circuit (AVar a) = (insert a (ANum resultA) circuitA, signal resultA)
  where (circuitA, resultA) = execExpr circuit (circuit ! a)
execExpr circuit (AAndB a b) = (circuitB, signal $ resultA .&. resultB)
  where (circuitA, resultA) = execExpr circuit a
        (circuitB, resultB) = execExpr circuitA b
execExpr circuit (AOrB a b) = (circuitB, signal $ resultA .|. resultB)
  where (circuitA, resultA) = execExpr circuit a
        (circuitB, resultB) = execExpr circuitA b
execExpr circuit (NotA a) = (circuitA, signal $ complement resultA)
  where (circuitA, resultA) = execExpr circuit a
execExpr circuit (ALShiftB a b) = (circuitA, signal $ resultA `shift` b)
  where (circuitA, resultA) = execExpr circuit a
execExpr circuit (ARShiftB a b) = (circuitA, signal $ resultA `shift` (- b))
  where (circuitA, resultA) = execExpr circuit a

addExpr :: Circuit -> String -> Circuit
addExpr circuit line = insert name ex circuit
  where (Wire name ex) = evalExpr line

readPuzzle :: Circuit -> [String] -> Circuit
readPuzzle circuit [] = circuit
readPuzzle circuit (line:rst) = readPuzzle (addExpr circuit line) rst

getSignal :: String -> Circuit -> Int
getSignal a circuit = snd $ execExpr circuit (circuit ! a)

main = do
  cnt <- getContents
--   print $ getSignal "d" $ readPuzzle M.empty $ lines cnt
--   print $ getSignal "e" $ readPuzzle M.empty $ lines cnt
--   print $ getSignal "f" $ readPuzzle M.empty $ lines cnt
--   print $ getSignal "g" $ readPuzzle M.empty $ lines cnt
--   print $ getSignal "h" $ readPuzzle M.empty $ lines cnt
--   print $ getSignal "i" $ readPuzzle M.empty $ lines cnt
--   print $ getSignal "x" $ readPuzzle M.empty $ lines cnt
--   print $ getSignal "y" $ readPuzzle M.empty $ lines cnt
  print $ getSignal "a" $ readPuzzle M.empty $ lines cnt

