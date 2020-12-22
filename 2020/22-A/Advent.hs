module Main where

import Data.List
import Data.List.Split

makeDecks ls = (head decks, head $ tail decks)
  where decks = map (map read . tail) $ splitOn [""] ls

play [] b = b
play a [] = a
play (a:as) (b:bs) | a < b = play as (bs ++ [b, a])
                   | otherwise = play (as ++ [a, b]) bs

calculateScore winner = foldl (+) 0 $ map (uncurry (*)) $ zip [1..] $ reverse winner

main = do
  cnt <- getContents
  print $ calculateScore $ uncurry play $ makeDecks $ lines cnt

