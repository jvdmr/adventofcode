module Main where

import Data.List hiding (find)
import Data.List.Split

import Debug.Trace
idtrace x = trace (show x) x
ftrace f x = trace (f x) x

data TargetType = BotT | OutputT
  deriving (Eq, Show)

data Target = Bot Int [Int]
            | Output Int [Int]
            deriving (Eq, Show, Ord)

data Instruction = Input Target Int
                 | Give Target Target Target
                 deriving (Eq, Show)

tid (Bot i _) = i
tid (Output i _) = i

pkg (Bot _ p) = p
pkg (Output _ p) = p

newtarget "bot" = flip Bot []
newtarget "output" = flip Output []

parseInstruction ["value", v, "goes", "to", "bot", b] = Input (newtarget "bot" $ read b) $ read v
parseInstruction ["bot", s, "gives", "low", "to", lt, ln, "and", "high", "to", ht, hn ] = Give (newtarget "bot" $ read s) (newtarget lt $ read ln) (newtarget ht $ read hn)

createOrReturn BotT n [] = Bot n [] 
createOrReturn OutputT n [] = Output n [] 
createOrReturn _ _ b = head b

matchT t n tgt = t == tt tgt && n == tid tgt

matchP p tgt = BotT == tt tgt && p == pkg tgt

find t bots n = createOrReturn t n $ dropWhile (not . matchT t n) bots

updateBot bots bot = bot:filter (not . matchT (tt bot) (tid bot)) bots

hasSpace (Bot _ ps) = length ps < 2
hasSpace _ = True

give (Bot n ps) v = Bot n $ sort (v:ps)
give (Output n ps) v = Output n $ sort (v:ps)

tt (Bot _ _) = BotT
tt (Output _ _) = OutputT

load (Bot _ ls) = ls

debug i bs = show i ++ " -> " ++ show bs

execute bots [] [] = bots
execute bots bi [] = execute' bots [] $ reverse bi
execute bots bi (i@(Input tgt v):ins) | hasSpace bot = execute' (updateBot bots (give bot v)) bi ins
                                      | otherwise = execute' bots (i:bi) ins
  where bot = find t bots n
        n = tid tgt
        t = tt tgt
execute bots bi (i@(Give src low high):ins) | not (hasSpace bot) && hasSpace botl && hasSpace both = execute' (updateBot (updateBot (updateBot bots (give both hv)) (give botl lv)) (newtarget "bot" s)) bi ins
                                            | otherwise = execute' bots (i:bi) ins
  where botl = find tl bots l
        both = find th bots h
        bot = find ts bots s
        [lv, hv] = load bot
        tl = tt low
        th = tt high
        ts = tt src
        l = tid low
        h = tid high
        s = tid src

execute' bots bi ins | [] /= result = result
                     | otherwise = execute bots bi ins
--   where result = filter (matchP [2, 5]) bots
  where result = filter (matchP [17, 61]) bots

main = do
  cnt <- getContents
  print $ execute' [] [] $ map (parseInstruction . splitOn " ") $ lines cnt

