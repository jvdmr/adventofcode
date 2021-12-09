module Main where

import Data.List hiding (empty, insert)
import Data.Map (insert, (!), keys)
import qualified Data.Map as M
import Text.Parsec
import Data.Functor.Identity

flatten :: [[a]] -> [a]
flatten = foldl (++) []

uniq [] = []
uniq [a] = [a]
uniq (a:b:rst) | a == b = uniq (b:rst)
               | otherwise = a:uniq (b:rst)

data Rule = Rule String String
  deriving (Eq, Show)

instance Ord Rule where
  compare (Rule a _) (Rule b _) = compare a b

rule =     many1 letter >>= \from -> string " => " >> many1 letter >>= return . Rule from
       <?> "rule"

parseRule :: String -> Rule
parseRule = right . parse rule "(source)"
  where right (Right ast) = ast
        right (Left a) = error $ show a

type Rules = M.Map String [String]

from :: String -> ParsecT String u Data.Functor.Identity.Identity String
from s =     try (string s >> return s)
         <?> "from"

inputParser :: Rules -> ParsecT String u Data.Functor.Identity.Identity String
inputParser rules =     (foldl (<|>) (head fromtos) (tail fromtos))
                    <|> (letter >>= return . (:[]))
                    <?> "input"
                    where fromtos = map from $ keys rules

parseInput :: Rules -> String -> [String]
parseInput rules = right . parse (many1 input) "(source)"
  where right (Right ast) = ast
        right (Left a) = error $ show a
        input = inputParser rules

(?!) :: (Ord a) => M.Map a [b] -> a -> [b]
(?!) m a = M.findWithDefault [] a m

compileRules :: Rules -> [Rule] -> Rules
compileRules ruleMap [] = ruleMap
compileRules ruleMap ((Rule from to):rules) = compileRules (insert from (to:(ruleMap ?! from)) ruleMap) rules

combineOutput :: Rules -> [String] -> [String]
combineOutput rules [] = [[]]
combineOutput rules (tos:otherTos) | tos `elem` (keys rules) = (map (tos ++) $ combineOutput rules otherTos) ++ (map (++ (foldl (++) "" otherTos)) $ rules ! tos)
                                   | otherwise = map (tos ++) $ combineOutput rules otherTos

applyRules :: [Rule] -> String -> [String]
applyRules rules input = uniq $ sort $ tail $ combineOutput ruleMap $ parseInput ruleMap input
  where ruleMap = compileRules M.empty rules

parseAndApplyRules :: [Rule] -> [String] -> [[String]]
parseAndApplyRules rules ("":inputs) = map (applyRules rules) inputs
parseAndApplyRules rules (rule:rst) = parseAndApplyRules (parsedRule:rules) rst
  where parsedRule = parseRule rule

main = do
  cnt <- getContents
  let result = parseAndApplyRules [] $ lines cnt
  print result
  print $ show $ map length $ result
