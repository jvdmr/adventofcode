module Main where

import Data.Map ((!), fromList, insert, delete, findWithDefault)
import qualified Data.Map as M
import Data.List hiding (insert, delete)
import Data.List.Split

import Debug.Trace
idtrace x = trace (show x) x
ftrace f x = trace (show $ f x) x

idtrace' :: (Show a, Show b) => M.Map a b -> M.Map a b
idtrace' = M.mapWithKey f
  where f a b = snd $ idtrace (a, b)

(!?) :: (Show k, Ord k, Show a) => M.Map k a -> k -> a
(!?) m k = ftrace f $ (M.!) m k
  where f a = (k, a)

data Action = Yell Int
            | Math String [String]
            deriving (Eq, Show, Ord)

terms :: Action -> [String]
terms (Math _ ts) = ts
terms _ = []

parseAction :: [String] -> Action
parseAction [x] = Yell $ read x
parseAction [a, o, b] = Math o [a, b]

parseMonkey :: [String] -> (String, Action)
parseMonkey [name, action] = (name, parseAction $ words action)

monkeys :: [String] -> M.Map String Action
monkeys = fromList . map (parseMonkey . splitOn ": ")

exe :: M.Map String Int -> Action -> Int
exe _ (Yell x) = x
exe ms (Math "+" [a, b]) = ms ! a + ms ! b
exe ms (Math "-" [a, b]) = ms ! a - ms ! b
exe ms (Math "*" [a, b]) = ms ! a * ms ! b
exe ms (Math "/" [a, b]) = ms ! a `div` ms ! b

switch' :: String -> Action -> [(String, Action)]
switch' name (Math "+" [a, b]) = [(a, Math "-" [name, b]), (b, Math "-" [name, a])]
switch' name (Math "-" [a, b]) = [(a, Math "+" [name, b]), (b, Math "-" [a, name])]
switch' name (Math "*" [a, b]) = [(a, Math "/" [name, b]), (b, Math "/" [name, a])]
switch' name (Math "/" [a, b]) = [(a, Math "*" [name, b]), (b, Math "/" [a, name])]

switch :: M.Map String Action -> String -> [(String, Action)]
switch ms name = switch' name $ ms ! name

solve :: M.Map String Action -> String -> Int
solve ms n = ms' ! n
  where ms' = fromList $ map calc $ M.keys ms
        calc name = (name, exe ms' (ms !? name))

contains :: M.Map String Action -> String -> String -> Bool
contains ms bad name | name == bad = True
                     | otherwise = any (contains ms bad) $ terms $ ms ! name

oper :: (Integral a, Num a) => String -> (a -> a -> a)
oper "+" = (+)
oper "-" = (-)
oper "*" = (*)
oper "/" = div

solve' :: M.Map String Action -> M.Map String Action -> String -> Int
solve' ms ms'' n = unroot $ ms'' !? n
  where unroot (Math op [a, b]) | all (contains ms "humn") [a, b] = oper op (solve' ms ms'' a) (solve' ms ms'' b)
                                | contains ms "humn" a = oper op (solve' ms ms'' a) (solve ms b)
                                | contains ms "humn" b = oper op (solve' ms ms'' b) (solve ms a)
                                | otherwise = solve ms n
        unroot (Yell x) = x

isMath :: Action -> Bool
isMath (Math _ _) = True
isMath _ = False

isYell :: Action -> Bool
isYell (Yell _) = True
isYell _ = False

pickYell :: Action -> Action -> Action
pickYell a@(Yell _) b@(Math _ _) = a
pickYell a@(Math _ _) b@(Yell _) = b

eq :: Eq b => (a -> b) -> a -> a -> Bool
eq f a b = f a == f b

fix :: M.Map String Action -> Int
fix ms = solve' ms ms'' "humn"
  where (Math _ [a, b]) = ms ! "root"
        ms' = insert "root" (Math "-" [a, b]) $ delete "humn" ms
        maths = M.filter isMath ms'
        switchmaths = fromList $ concat $ map (switch maths) $ M.keys maths
        ms'' = insert "root" (Yell 0) switchmaths

test :: M.Map String Action -> Int -> Int
test ms humn = solve ms' "root"
  where (Math _ [a, b]) = ms ! "root"
        ms' = insert "root" (Math "-" [a, b]) $ insert "humn" (Yell humn) ms

main = do
  cnt <- getContents
  let ms = monkeys $ lines cnt
--   print $ fix ms
  print $ test ms $ fix ms

