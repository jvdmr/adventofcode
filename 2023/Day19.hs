{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module Day19
  ( part1
  , part2
  ) where

import Data.List
import Data.List.Split (splitOn)
import Data.Map ((!))
import qualified Data.Map as M

import Vdmr.Generic
import Vdmr.Parsec

data MachinePart = MP { x :: Int, m :: Int, a :: Int, s :: Int }
  deriving (Show, Eq, Read)

type MachineParts = [MachinePart]

getProp :: Char -> MachinePart -> Int
getProp 'x' = x
getProp 'm' = m
getProp 'a' = a
getProp 's' = s

data WFName = Accepted
            | Rejected
            | Label String
            deriving (Show, Eq, Ord)

type RuleCheck = (Char, Char, Int)

data Rule = Rule [RuleCheck] WFName

name :: Rule -> WFName
name (Rule _ n) = n

checks :: Rule -> [RuleCheck]
checks (Rule rc _) = rc

type Rules = [Rule]

type Workflow = (WFName, Rules)

type Workflows = M.Map WFName Rules

check :: MachinePart -> RuleCheck -> Bool
check _ (_, 'x', _) = True
check mp (prop, '>', n) = getProp prop mp > n
check mp (prop, '<', n) = getProp prop mp < n

defaultRule :: RuleCheck
defaultRule = ('x', 'x', 0)

switch :: RuleCheck -> RuleCheck
switch (prop, '<', n) = (prop, '>', n - 1)
switch (prop, '>', n) = (prop, '<', n + 1)
switch (prop, 'x', n) = (prop, 'x', n)

wfname :: Stream s m Char => ParsecT s u m WFName
wfname =     (char 'A' >> return Accepted)
         <|> (char 'R' >> return Rejected)
         <|> (many1 letter >>= return . Label)
         <?> "wfname"

rule :: Stream s m Char => ParsecT s u m Rule
rule =     try (oneOf "xmas" >>= \p -> oneOf "<>" >>= \op -> many1 digit >>= \n -> char ':' >> wfname >>= return . Rule [(p, op, read n)])
       <|> (wfname >>= return . Rule [defaultRule])
       <?> "rule"

workflow :: Stream s m Char => ParsecT s u m Workflow
workflow =     (wfname >>= \n -> braces (commaSep rule) >>= return . (,) n)
           <?> "workflow"

expandRules :: Workflow -> Workflow
expandRules (n, rs) = (n, rs')
  where rs' = reverse $ expand $ reverse rs
        expand [] = []
        expand (Rule rc t:rst) = Rule (rc ++ (map switch $ flatten $ map checks rst)) t:expand rst

parseWorkflow :: String -> Workflow
parseWorkflow = expandRules . right . parse workflow "(source)"
  where right (Right ast) = ast
        right (Left x) = error $ show x

readMachinePart :: String -> MachinePart
readMachinePart = read . ("MP"++)

parseInput :: [String] -> (Workflows, MachineParts)
parseInput ss = (M.fromList $ map parseWorkflow wfs, map readMachinePart mps)
  where [wfs, mps] = splitOn [""] ss

accept :: Workflows -> WFName -> MachinePart -> Bool
accept _ Accepted _ = True
accept _ Rejected _ = False
accept wfs n mp = accept wfs nxt mp
  where rs = wfs ! n
        eval (Rule rf _) = all (check mp) rf
        nxt = name $ head $ filter eval rs

goodParts :: (Workflows, MachineParts) -> MachineParts
goodParts (wfs, mps) = filter (accept wfs (Label "in")) mps

rate :: MachinePart -> Int
rate (MP x m a s) = x + m + a + s

part1 :: Solver
part1 = show . sum . map rate . goodParts . parseInput . lines

type Bounds = (Int, Int)
type PossibleParts = M.Map Char Bounds

allParts :: PossibleParts
allParts = M.fromList $ map (flip (,) (0, 4000)) "xmas"

countParts :: PossibleParts -> Int
countParts parts = product $ map (uncurry (flip (-)) . (parts !)) "xmas"

gt :: Int -> Bounds -> Bounds
gt n (a, b) | a > n = (a, b)
            | b < n = (n, n)
            | otherwise = (n, b)

lt :: Int -> Bounds -> Bounds
lt n (a, b) | b < n = (a, b)
            | a > n = (n, n)
            | otherwise = (a, n - 1)

apply :: PossibleParts -> [RuleCheck] -> PossibleParts
apply parts = foldl apply' parts
  where apply' ps (prop, '>', n) = M.insert prop (gt n $ ps ! prop) ps
        apply' ps (prop, '<', n) = M.insert prop (lt n $ ps ! prop) ps
        apply' ps (_, 'x', _) = ps

possibleParts :: Workflows -> Int
possibleParts wfs = sum $ map (countParts . apply allParts . flatten . map snd) $ filter accepted $ bfs nb id [] [[(Label "in", [defaultRule])]]
  where nb ((Accepted, _):_) = []
        nb ((Rejected, _):_) = []
        nb rest@((n, _):_) = map ((:rest) . ruleToBounds) $ wfs ! n
        ruleToBounds (Rule rc n) = (n, rc)
        accepted ((Accepted, _):_) = True
        accepted _ = False

part2 :: Solver
part2 = show . possibleParts . fst . parseInput . lines

