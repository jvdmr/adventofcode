module Main where

import Data.Map ((!), fromList)
import qualified Data.Map as M
import Data.List
import Data.List.Split

import Debug.Trace
idtrace x = trace (show x) x

data Action = Yell Int
            | Math String String String
            deriving (Eq, Show)

parseAction :: [String] -> Action
parseAction [x] = Yell $ read x
parseAction [a, o, b] = Math o a b

parseMonkey :: [String] -> (String, Action)
parseMonkey [name, action] = (name, parseAction $ words action)

monkeys :: [String] -> M.Map String Action
monkeys = fromList . map (parseMonkey . splitOn ": ")

exe :: String -> Int -> Int -> Int
exe "+" a b = a + b
exe "-" a b = a - b
exe "*" a b = a * b
exe "/" a b = a `div` b

data Expr = Val Int
          | Expr (Int -> Int)

showExpr (Val n) = n

andThen a (Expr b) = Expr (b . a)

switch :: String -> Expr -> Expr -> Expr
switch op (Val a) (Val b) = Val $ exe op a b
switch "+" (Val a) o = andThen (subtract a) o
switch "+" o (Val a) = andThen (subtract a) o
switch "-" (Val a) o = andThen (a -) o
switch "-" o (Val a) = andThen (a +) o
switch "*" (Val a) o = andThen (flip div a) o
switch "*" o (Val a) = andThen (flip div a) o
switch "/" (Val a) o = andThen (div a) o
switch "/" o (Val a) = andThen (a *) o
switch "=" (Val a) (Expr e) = Val $ e a
switch "=" (Expr e) (Val a) = Val $ e a

build _ _ (Yell x) = Val x
build ms n (Math op a b) | n == "root" = switch "=" l r
                         | otherwise = switch op l r
  where l = solve a ms
        r = solve b ms

solve :: String -> M.Map String Action -> Expr
solve "humn" _ = Expr id
solve n ms = build ms n $ ms ! n

main = do
  cnt <- getContents
  print $ showExpr $ solve "root" $ monkeys $ lines cnt

