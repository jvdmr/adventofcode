module Main where

import Data.List hiding (insert)
import Data.List.Split
import Data.HashMap hiding (map, filter)

parseBag bag = unwords $ init $ words bag

parseBags :: String -> Maybe String
parseBags (_:' ':bag) = Just $ parseBag bag
parseBags "no other bags" = Nothing

insertRule parent rules (Just bag) = insert bag (nub $ sort $ parent:findWithDefault [] bag rules) rules
insertRule parent rules Nothing = rules

parseRule rules (parent:bags:[]) = foldl (insertRule $ parseBag parent) rules $ map parseBags $ splitOn ", " bags

bfs :: String -> [String] -> [String] -> [String] -> Map String [String] -> [String]
bfs _ result [] _ _ = result
bfs color result (q:queue) seen graph = bfs color newResult newQueue newSeen graph
  where newQueue = filter (not . (flip elem $ seen)) $ nub $ sort $ (queue ++ (findWithDefault [] q graph))
        newSeen = q:seen
        newResult = filter (/= color) $ nub $ sort $ q:result

findColor = "shiny gold"

main = do
  cnt <- getContents
  print $ length $ bfs findColor [] [findColor] [] $ foldl parseRule empty $ map (splitOn " contain " . init) $ lines cnt

