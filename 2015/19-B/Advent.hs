module Main where

import Data.List hiding (empty, insert)
import Data.List.Split
import Data.Map (insert, (!), keys, elems, assocs)
import qualified Data.Map as M
import Text.Parsec
-- import Data.Functor.Identity

import Debug.Trace
idtrace :: (Show a) => a -> a
idtrace s = trace (show s) s

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

reverseRule :: String -> String -> Rule
reverseRule from to = Rule to from

rule =     many1 letter >>= \from -> string " => " >> many1 letter >>= return . reverseRule from
       <?> "rule"

parseRule :: String -> Rule
parseRule = right . parse rule "(source)"
  where right (Right ast) = ast
        right (Left a) = error $ show a

type Rules = M.Map String String

compareLength :: (String, a) -> (String, a) -> Ordering
compareLength (a, _) (b, _) = compare (length b) (length a)

compileRules :: Rules -> [Rule] -> Rules
compileRules ruleMap [] = ruleMap
compileRules ruleMap ((Rule from to):rules) = compileRules (insert from to ruleMap) rules

sublists :: (Eq a) => [a] -> [[a]]
sublists l = filter valid $ inits l ++ tails l ++ (foldl (++) [] $ map inits $ tails l)
  where valid [] = False
        valid lt | l == lt = False
                 | otherwise = True

replaceInstances :: (Show a, Eq a) => [a] -> [a] -> [a] -> [[a]]
replaceInstances ss nss s = permutate $ splitOn ss s
  where permutate [a] = [a]
        permutate (a:rest) = (a ++ nss ++ intercalate ss rest):(map ((a ++ ss) ++) $ permutate rest)

fstLength (a, _) (b, _) = compare (length a) (length b)

applyRules :: Rules -> [String] -> [(String, Int)] -> Int
applyRules _ _ [] = error "this should not happen"
applyRules _ _ (("e", n):_) = n
applyRules ruleMap seen ((s, n):rest) | elem s (keys ruleMap) = applyRules ruleMap (s:seen) $ (ruleMap ! s, n + 1):rest
                                      | otherwise = applyRules ruleMap (s:seen) $ sortBy fstLength $ (map (\rs -> idtrace (rs, n + 1)) $ filter (not . flip elem (seen ++ map fst rest)) $ flatten $ map applyRule $ uniq $ sort $ filter (flip elem safeKeys) $ sublists s) ++ rest
                                      where applyRule ss = replaceInstances ss (ruleMap ! ss) s
                                            safeKeys = filter (("e" /=) . (ruleMap !)) $ keys ruleMap

parseAndApplyRules :: [Rule] -> [String] -> [Int]
parseAndApplyRules rules ("":inputs) = map (applyRules ruleMap [] . (:[]) . (\i -> (i, 0))) inputs
  where ruleMap = compileRules M.empty rules
parseAndApplyRules rules (rule:rst) = parseAndApplyRules (parsedRule:rules) rst
  where parsedRule = parseRule rule

main = do
  cnt <- getContents
  print $ parseAndApplyRules [] $ lines cnt
