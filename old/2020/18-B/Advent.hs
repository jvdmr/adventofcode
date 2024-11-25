module Main where

-- import Data.List
import Data.Char

import Text.Parsec
import Text.PrettyPrint (($$), text)

data Expr = Plus Expr Expr
          | Multiply Expr Expr
          | Parens Expr
          | Number Int
          deriving (Eq)

instance Show Expr where
  show (Parens a) = show a
  show (Plus a b) = "(" ++ show a ++ "+" ++ show b ++ ")"
  show (Multiply a b) = "(" ++ show a ++ "*" ++ show b ++ ")"
  show (Number a) = show a

nospaces [] = []
nospaces (' ':rst) = nospaces rst
nospaces (a:rst) = a:nospaces rst

parens = char '(' >> expr >>= \e -> char ')' >> (return $ Parens e)

number =     (digit >>= return . Number . read . (:[]))
         <|> parens
         <?> "number"

plus =     try (number >>= \a -> char '+' >> plus >>= return . Plus a)
       <|> number
       <?> "plus"

multiply =     try (plus >>= \a -> char '*' >> multiply >>= return . Multiply a)
           <|> plus
           <?> "multiply"

expr =     multiply
       <?> "expr"

tokenize = right . parse expr "(source)"
  where right (Right ast) = ast

fakeMath (Parens e) = fakeMath e
fakeMath (Plus a b) = fakeMath a + fakeMath b
fakeMath (Multiply a b) = fakeMath a * fakeMath b
fakeMath (Number a) = a

main = do
  cnt <- getContents
--   print $ foldl ($$) (text "") $ map (text . show . tokenize . nospaces) $ lines cnt
--   print $ map (fakeMath . tokenize . nospaces) $ lines cnt
  print $ foldl (+) 0 $ map (fakeMath . tokenize . nospaces) $ lines cnt

