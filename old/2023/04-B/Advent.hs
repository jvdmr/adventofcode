module Main where

-- import Data.List
import Data.List.Split (splitOn)
import Data.Map ((!))
import qualified Data.Map as M

import Debug.Trace
idtrace x = trace (show x) x

type Card = ([Int], [Int])
type Scores = M.Map Int Int

parseCard :: String -> (Int, Card)
parseCard s = (read n, (head nrs, last nrs))
  where nrs = map (map read . filter ((<) 0 . length) . splitOn " ") $ splitOn " | " content
        ['C':'a':'r':'d':' ':n, content] = splitOn ": " s

calculate :: Scores -> (Int, Card) -> Scores
calculate scores (n, (winning, ours)) = M.insert n (1 + score) scores
  where matching = length $ filter (flip elem winning) ours
        extraCards = filter (flip elem $ M.keys scores) $ take matching $ tail [n..]
        score = sum $ map ((!) scores) extraCards

parseCards :: [String] -> Scores
parseCards = foldl calculate M.empty . reverse . map parseCard

totalScore :: Scores -> Int
totalScore s = sum $ M.elems s

main = do
  cnt <- getContents
  print $ totalScore $ parseCards $ lines cnt
