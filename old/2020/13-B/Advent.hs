module Main where

import Data.List
import Data.List.Split

data Bus = Bus {index::Int, id::Int}
         | Skip {value::Int}
         deriving (Show, Eq)

compareBus _ (Skip _) = LT
compareBus (Skip _) _ = GT
compareBus (Bus _ a) (Bus _ b) = compare b a

busify :: [Bus] -> String -> [Bus]
busify ((Skip value):buses) "x" = (Skip (value+1)):buses
busify buses "x" = (Skip 1):buses
busify buses bus = (Bus index $ read bus):buses
  where index = foldl countBus 0 buses
        countBus i (Bus _ _) = i+1
        countBus i (Skip v) = i+v

streak matched _ [] = matched
streak matched n ((Skip v):buses) = streak matched (n+v) buses
streak matched n ((Bus _ b):buses) | mod n b == 0 = streak (b:matched) (n+1) buses
                                   | otherwise = streak matched (n+1) buses

countBuses [] = 0
countBuses ((Skip _):rst) = countBuses rst
countBuses ((Bus _ _):rst) = 1 + countBuses rst

findStreak n matched buses | nBuses == length newMatched = n
                           | otherwise = findStreak (n+next) newMatched buses
  where next = foldl (*) 1 newMatched
        newMatched = nub $ sort $ streak matched n buses
        nBuses = countBuses buses + 1 -- because we start with [1]

findBus :: [String] -> Int
findBus (_:schedule:[]) = findStreak 0 [1] buses
  where buses = reverse $ foldl busify [] $ splitOn "," schedule

main = do
  cnt <- getContents
  print $ findBus $ lines cnt

