module Main where

-- import Data.List

import Debug.Trace
idtrace x = trace (show x) x

data RPS = Rock | Paper | Scissors
  deriving (Eq, Show)

rps Rock Paper = 6
rps Paper Scissors = 6
rps Scissors Rock = 6
rps Paper Rock = 0
rps Scissors Paper = 0
rps Rock Scissors = 0
rps _ _ = 3

score Rock = 1
score Paper = 2
score Scissors = 3

parseRps 'A' = Rock
parseRps 'B' = Paper
parseRps 'C' = Scissors
parseRps 'X' = Rock
parseRps 'Y' = Paper
parseRps 'Z' = Scissors

rpsLine [a, ' ', b] = score y + rps x y
  where x = parseRps a
        y = parseRps b

main = do
  cnt <- getContents
  print $ foldl (+) 0 $ map rpsLine $ lines cnt

