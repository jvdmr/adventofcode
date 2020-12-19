module Main where

import Data.List hiding (insert)
import Data.Char
import Data.Map (insert, (!))
import qualified Data.Map as T

import Text.Parsec
import Text.PrettyPrint (($$), text)

join j (s:ss) = foldl strBetween s ss
  where strBetween a b = a ++ j ++ b

data Expr = AChar Char
          | AThenB [Int]
          | AOrB Expr Expr
          deriving (Eq)

instance Show Expr where
  show (AChar a) = '"':a:'"':[]
  show (AThenB abs) = join " " $ map show abs
  show (AOrB a b) = "(" ++ show a ++ " | " ++ show b ++ ")"

addA a (AThenB b) = AThenB $ a:b

data Rule = Rule Int Expr
          deriving (Show, Eq)

quotes =     char '"' >> letter >>= \c -> char '"' >> (return $ AChar c)
         <?> "quotes"

athenb =     try (many1 digit >>= \a -> space >> athenb >>= return . addA (read a))
         <|> (many1 digit >>= return . AThenB . (:[]) . read)

aorb =     athenb >>= \a -> space >> char '|' >> space >> athenb >>= return . AOrB a

expr =     try (aorb)
       <|> athenb
       <|> quotes
       <?> "expr"

rule =     many1 digit >>= \i -> char ':' >> space >> expr >>= return . Rule (read i)
       <?> "rule"

astRule = right . parse rule "(source)"
  where right (Right ast) = ast

parseRule rules line = insert i e rules
  where (Rule i e) = astRule line

createAst _ (AChar c) = char c
createAst rules (AThenB [a]) = createAst rules (rules ! a)
createAst rules (AThenB (a:bs)) = createAst rules (rules ! a) >> createAst rules (AThenB bs)
createAst rules (AOrB a b) = try (createAst rules a) <|> createAst rules b

parseAndFilter ls = filter (acceptedByAst . parse ast "(source)") $ tail $ dropWhile (/= "") ls
  where rules = foldl parseRule T.empty $ takeWhile (/= "") ls
        ast = createAst rules (rules ! 0) >> lookAhead eof
        acceptedByAst (Left e) = False
        acceptedByAst (Right _) = True
        
main = do
  cnt <- getContents
  print $ foldl ($$) (text "") $ map text $ parseAndFilter $ lines cnt
  print $ length $ parseAndFilter $ lines cnt

