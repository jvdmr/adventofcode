module Main where

import Data.List
import Data.List.Split

calculate :: [String] -> [[(Int, Int)]]
calculate [] = []
calculate ("x":rst) = calculate rst
calculate (bus:rst) = (map ((,) ibus) [0, ibus ..]):calculate rst
  where ibus = read bus

compareDepartures :: [(Int, Int)] -> [(Int, Int)] -> Ordering
compareDepartures ((_, timeA):_) ((_, timeB):_) = compare timeA timeB

merge :: [[(Int, Int)]] -> [(Int, Int)]
merge lists = (head firstList):(merge $ (tail firstList):otherLists)
  where (firstList:otherLists) = sortBy compareDepartures lists

checkSum :: Int -> (Int, Int) -> Int
checkSum now (id, time) = id * (time - now)

findBus :: [String] -> Int
findBus (now:schedule:[]) = checkSum inow $ head $ dropWhile ((< inow) . snd) $ merge $ calculate $ splitOn "," schedule
  where inow = read now

main = do
  cnt <- getContents
  print $ findBus $ lines cnt

