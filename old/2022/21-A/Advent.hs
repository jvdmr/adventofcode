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

exe :: M.Map String Int -> Action -> Int
exe ms' (Yell x) = x
exe ms' (Math "+" a b) = ms' ! a + ms' ! b
exe ms' (Math "-" a b) = ms' ! a - ms' ! b
exe ms' (Math "*" a b) = ms' ! a * ms' ! b
exe ms' (Math "/" a b) = ms' ! a `div` ms' ! b

solve :: M.Map String Action -> Int
solve ms = ms' ! "root"
  where ms' = fromList $ map calc $ M.keys ms
        calc name = (name, exe ms' (ms ! name))

main = do
  cnt <- getContents
  print $ solve $ monkeys $ lines cnt

