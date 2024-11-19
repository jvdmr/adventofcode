{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC2023.Day20
  ( part1
  , part2
  ) where

import Data.List
import Data.Map ((!))
import qualified Data.Map as M

import Vdmr.Generic (Solver, none, bfsState)
import Vdmr.Grid (add)
import Vdmr.Parsec

data Pulse = High
           | Low
           | None
           deriving (Show, Eq)

data MState = On
            | Off
            deriving (Show, Eq)

type MName = String
type Targets = [MName]
type Memory = M.Map MName Pulse

data Module = Broadcaster Targets
            | FlipFlop MState Targets
            | Conjunction Memory Targets
            deriving (Show, Eq)

isBroadcaster :: Module -> Bool
isBroadcaster (Broadcaster _) = True
isBroadcaster _ = False

isFlipFlop :: Module -> Bool
isFlipFlop (FlipFlop _ _) = True
isFlipFlop _ = False

isConjunction :: Module -> Bool
isConjunction (Conjunction _ _) = True
isConjunction _ = False

tgts :: Module -> Targets
tgts (Broadcaster ts) = ts
tgts (FlipFlop _ ts) = ts
tgts (Conjunction _ ts) = ts

cleanMemory :: Memory
cleanMemory = M.empty

mname :: Stream s m Char => ParsecT s u m MName
mname =     (optional space >> many1 letter >>= return)
        <?> "mname"

targets :: Stream s m Char => ParsecT s u m Targets
targets =     (string " -> " >> commaSep mname >>= return)
          <?> "targets"

mdl :: Stream s m Char => ParsecT s u m (MName, Module)
mdl =     (string "broadcaster" >> targets >>= return . (,) "broadcaster" . Broadcaster)
      <|> (char '%' >> mname >>= \n -> targets >>= return . (,) n . FlipFlop Off)
      <|> (char '&' >> mname >>= \n -> targets >>= return . (,) n . Conjunction cleanMemory)
      <?> "mdl"

type Network = M.Map MName Module

(!?) :: Network -> MName -> Module
(!?) ntw name | M.member name ntw = ntw ! name
              | otherwise = Broadcaster []

type Sources = [MName]

sources :: MName -> Network -> Sources
sources name = map fst . filter (elem name . tgts . snd) . M.toList

fixConjunctions :: Network -> Network
fixConjunctions ntw = M.fromList $ map fix $ M.toList ntw
  where fix (n, Conjunction _ ts) = (n, Conjunction (mem n) ts)
        fix other = other
        mem n = M.fromList $ map (flip (,) Low) $ sources n ntw

parseInput :: [String] -> Network
parseInput = fixConjunctions . M.fromList . map (right . parse mdl "(source)")
  where right (Right ast) = ast
        right (Left x) = error $ show x

send :: (Pulse, MName) -> Module -> (Module, Pulse)
send (p, _) mdl@(Broadcaster _) = (mdl, p)
send (High, _) mdl@(FlipFlop _ _) = (mdl, None)
send (Low, _) (FlipFlop Off ts) = (FlipFlop On ts, High)
send (Low, _) (FlipFlop On ts) = (FlipFlop Off ts, Low)
send (p, from) (Conjunction mem ts) = (Conjunction mem' ts, p')
  where mem' = M.insert from p mem
        p' = if (all (== High) $ M.elems mem') then Low else High

neighbors :: Network -> SentPulse -> (Network, [SentPulse])
neighbors ntw (i, (name, p)) = (M.insert name mdl ntw, nxt)
  where (mdl, p') = send p $ ntw !? name
        nxt = if p' == None then [] else map sendPulse $ tgts mdl
        sendPulse t = (i + 1, (t, (p', name)))

type SentPulse = (Int, (MName, (Pulse, MName)))

buttonPulse :: (Network, [SentPulse]) -> (Network, [SentPulse])
buttonPulse (ntw, _) = (ntw', pulses')
  where (ntw', pulses') = bfsState ntw neighbors id [] [(0, ("broadcaster", (Low, "button")))]

countPulses :: [SentPulse] -> (Int, Int)
countPulses lst = (lows, highs)
  where lows = length $ filter ((== Low) . fst . snd . snd) lst
        highs = length $ filter ((== High) . fst . snd . snd) lst

showPulse :: SentPulse -> String
showPulse (i, (to, (p, from))) = show i ++ " :: " ++ from ++ " -" ++ show p ++ "-> " ++ to

part1 :: Solver
part1 = show . uncurry (*) . countPulses . reverse . concat . map snd . take 1001 . iterate buttonPulse . flip (,) [] . parseInput . lines

data CycleNodeType = Sending
                   | Receiving
                   | Both
                   deriving (Show, Eq)

cyclePulse :: (Network -> SentPulse -> (Network, [SentPulse])) -> [SentPulse] -> (Network, [SentPulse]) -> (Network, [SentPulse])
cyclePulse nbf pulses (ntw, _) = (ntw', pulses')
  where (ntw', pulses') = bfsState ntw nbf id [] pulses

cycleNb :: MName -> Network -> SentPulse -> (Network, [SentPulse])
cycleNb conj ntw p@(i, (to, (Low, from))) | conj == from = (ntw, [])
                                          | otherwise = neighbors ntw p
cycleNb _ ntw p = neighbors ntw p

countCycle :: Network -> MName -> Int
countCycle ntw name = length $ takeWhile noLowConj $ iterate pulse' (ntw, [])
  where mdl = ntw ! name
        conj = head $ filter (isConjunction . (ntw !)) $ tgts mdl
        nb = cycleNb conj
        pulse' = cyclePulse nb [(0, (name, (Low, "broadcaster")))]
        noLowConj (_, ps) = none lowConj ps
        lowConj (_, (_, (Low, from))) = from == conj
        lowConj _ = False

cycles :: Network -> [Int]
cycles ntw | hasRx = map (countCycle ntw) $ tgts $ ntw ! "broadcaster"
           | otherwise = [1]
           where hasRx = elem "rx" $ concat $ map tgts $ M.elems ntw

part2 :: Solver
part2 = show . foldl lcm 1 . cycles . parseInput . lines

