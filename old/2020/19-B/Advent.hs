module Main where

import Debug.Trace

import Data.List hiding (insert)
import Data.Char
import Data.Map (insert, (!))
import qualified Data.Map as T

import Text.Parsec
import Text.PrettyPrint (($$), text)

import Control.Monad.Identity (Identity)

idtrace x = trace (show x) x
trytrace x = trace ("try: " ++ show x) x
valtrace x = trace (show $ foldl ($$) (text "") $ map (text . show . ((!) x)) $ T.keys x) x

join j (s:ss) = foldl strBetween s ss
  where strBetween a b = a ++ j ++ b

data Expr = AChar Char
          | AThenB [Int]
          | AOrB Expr Expr
          deriving (Eq)

instance Show Expr where
  show (AChar a) = '"':a:'"':[]
  show (AThenB as) = join " " $ map show as
  show (AOrB a b) = "(" ++ show a ++ " | " ++ show b ++ ")"

addA a (AThenB b) = AThenB $ a:b

data Rule = Rule Int Expr
          deriving (Eq)

instance Show Rule where
  show (Rule i e) = show i ++ ": " ++ show e

quotes =     char '"' >> letter >>= \c -> char '"' >> return (AChar c)
         <?> "quotes"

ref =     many1 digit >>= return . read
      <?> "ref"

athenb =     try (ref >>= \a -> space >> athenb >>= return . addA a)
         <|> (ref >>= return . AThenB . (:[]))
         <?> "athenb"

aorb =     athenb >>= \a -> space >> char '|' >> space >> athenb >>= return . AOrB a
       <?> "aorb"

expr =     try (aorb)
       <|> athenb
       <|> quotes
       <?> "expr"

rule =     many1 digit >>= \i -> char ':' >> space >> expr >>= return . Rule (read i)
       <?> "rule"

astRule = right . parse rule "(source)"
  where right (Right ast) = ast

parseRule rules line = insert i r rules
  where r@(Rule i _) = astRule line

createAst :: (Int -> ParsecT [Char] u Identity () -> ParsecT [Char] u Identity ()) -> Rule -> ParsecT [Char] u Identity () -> ParsecT [Char] u Identity ()
createAst _ (Rule _ (AChar a)) rest = char a >> rest
createAst lookup (Rule i (AThenB [a])) rest = lookup a rest
createAst lookup (Rule i (AThenB as)) rest = lookup (head as) $ createAst lookup (Rule i $ AThenB $ tail as) rest
createAst lookup (Rule i (AOrB a b)) rest = try (createAst lookup (Rule i a) rest) <|> createAst lookup (Rule i b) rest

parseAndFilter ls = filter (acceptedByAst . parse parser "(source)") $ tail $ dropWhile (/= "") ls
  where rules = foldl parseRule T.empty $ takeWhile (/= "") ls
        ast = T.map (createAst execute) rules
        parser = execute 0 $ lookAhead eof
        execute i = ast ! i
        acceptedByAst (Left e) = False
        acceptedByAst (Right _) = True
        
main = do
  cnt <- getContents
  print $ foldl ($$) (text "") $ map text $ parseAndFilter $ lines cnt
  print $ length $ parseAndFilter $ lines cnt

