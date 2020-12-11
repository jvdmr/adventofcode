module Main where

import Data.List hiding (insert)
import Data.List.Split
import Data.HashMap hiding (map, filter)

flatten :: [[a]] -> [a]
flatten [] = []
flatten (l:ls) = l ++ flatten ls

data Bag = Bag {amount::Int, color::String}
         | NoBag
         deriving (Eq, Ord, Show)

parseBag num bag = Bag num $ unwords $ init $ words bag

parseBags :: String -> Bag
parseBags (num:' ':bag) = parseBag (read $ num:[]) bag
parseBags "no other bags" = NoBag

insertRule parent rules [NoBag] = rules
insertRule parent rules bags = insert parent bags rules

parseRule rules (parent:bags:[]) = insertRule (color $ parseBag 0 parent) rules $ map parseBags $ splitOn ", " bags

bagsInThisBag bag rules = flatten $ take (amount bag) $ iterate id $ findWithDefault [] (color bag) rules

bfs :: [Bag] -> [Bag] -> Map String [Bag] -> [Bag]
bfs result [] _ = result
bfs result (bag:queue) graph = bfs newResult newQueue graph
  where newQueue = queue ++ bagsInThisBag bag graph
        newResult = bag:result

findColor = Bag 1 "shiny gold"

main = do
  cnt <- getContents
  print $ foldl (+) 0 $ map amount $ init $ bfs [] [findColor] $ foldl parseRule empty $ map (splitOn " contain " . init) $ lines cnt

