module Main where

import Data.List

import Debug.Trace
idtrace x = trace (show x) x

data RPS = Rock | Paper | Scissors | Lose | Draw | Win
  deriving (Eq, Show)

parseRps :: Char -> RPS
parseRps 'A' = Rock
parseRps 'B' = Paper
parseRps 'C' = Scissors
parseRps 'X' = Lose
parseRps 'Y' = Draw
parseRps 'Z' = Win

score :: RPS -> Int
score Rock = 1
score Paper = 2
score Scissors = 3
score Lose = 0
score Draw = 3
score Win = 6

choice :: RPS -> RPS -> RPS
choice Rock Lose = Scissors
choice Paper Lose = Rock
choice Scissors Lose = Paper
choice Rock Win = Paper
choice Paper Win = Scissors
choice Scissors Win = Rock
choice a Draw = a

rpsLine :: String -> Int
rpsLine [a, ' ', b] = score y + score (choice x y)
  where x = parseRps a
        y = parseRps b

main = do
  cnt <- getContents
  print $ foldl (+) 0 $ map rpsLine $ lines cnt

