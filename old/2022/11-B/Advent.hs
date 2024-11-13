module Main where

import Prelude hiding (round)
import Data.List
import Data.List.Split

import Debug.Trace
idtrace x = trace (show x) x
ftrace f x = trace (f x) x

showMonkey (Monkey n id is _ _) = "Monkey " ++ show id ++ ":\t" ++ show n ++ " times - " ++ show is

data Test = Test Int Int Int
  deriving (Eq, Show, Ord)

data Monkey = Monkey Int Int [Int] [String] Test
  deriving (Eq, Show, Ord)

testItem :: Test -> Int -> Int
testItem (Test d t f) item | mod item d == 0 = t
                           | otherwise = f

parseTest :: [[String]] -> Test
parseTest [("Test:":divby), iftrue, iffalse] = Test (read $ last divby) (read $ last iftrue) (read $ last iffalse)

parseDetails :: [[String]] -> ([Int] -> [String] -> Test -> Monkey) -> Monkey
parseDetails (("Starting":"items:":items):("Operation:":"new":"=":f):test) m = m (map read $ splitOn "," $ concat items) f (parseTest test)

parseMonkey :: [[String]] -> Monkey
parseMonkey (["Monkey", id]:details) = parseDetails details $ Monkey 0 (read $ init id)

applyOperation :: [String] -> Int -> Int
applyOperation ["old", "+", "old"] = (*2)
applyOperation ["old", "*", "old"] = (^2)
applyOperation ["old", "+", num] = (+) (read num)
applyOperation ["old", "*", num] = (*) (read num)

testMod (Monkey _ _ _ _ (Test d _ _)) = d

give :: [Int] -> Test -> Monkey -> Monkey
give is t (Monkey n id mis mf mt) = Monkey n id (mis ++ (filter thisOne is)) mf mt
  where thisOne i = id == testItem t i

turn :: Int -> [Monkey] -> [Monkey]
turn superMod (m@(Monkey n id is f t):monkeys) = m:turn superMod ((map (give inspected t) monkeys) ++ [Monkey (n + length is) id [] f t])
  where inspected = map (superM . applyOperation f) is
        superM x = mod x superMod

rounds :: Int -> [Monkey] -> [Monkey]
rounds n monkeys = (chunksOf (length monkeys) $ turn superMod monkeys) !! n
  where superMod = foldl (*) 1 $ map testMod monkeys

monkeyBusiness :: [Monkey] -> Int
monkeyBusiness monkeys = a * b
  where (a:b:_) = reverse $ sort inspections
        inspections = map inspection monkeys
        inspection (Monkey n _ _ _ _) = n

main = do
  cnt <- getContents
  print $ monkeyBusiness $ map idtrace $ rounds 10000 $ map parseMonkey $ splitOn [[]] $ map words $ lines cnt

