module Main where

import Data.Char (toUpper)
import Data.Ord (comparing)
import Data.List
import Data.List.Split
import Data.Map ((!), fromList)
import qualified Data.Map as M

import Debug.Trace
idtrace x = trace (show x) x
ftrace f x = trace (show $ f x) x
stateTrace state = trace (pp 0 state) state

-- pp x (State (n, r, i, Nothing)) = (take x $ repeat ' ') ++ show (n, r, i) ++ "\n"
-- pp x (State (n, r, i, Just s)) = (take x $ repeat ' ') ++ show (n, r, i) ++ "\n" ++ pp (x + 1) s
pp _ (State (n, r, i, _)) = show (n, r, i)
-- pp _ (State s) = show s

uniqF :: (a -> a -> Bool) -> [a] -> [a]
uniqF f = map head . groupBy f

data Resource = Ore | Clay | Obsidian | Geode
  deriving (Eq, Show, Read, Ord)

type Resources = M.Map Resource Int

zero :: Resources
zero = fromList $ zip [Ore, Clay, Obsidian, Geode] [0, 0..]

data Plan = Plan {kind::Resource, req::Resources}
  deriving (Eq, Show)

data Blueprint = Blueprint Int [Plan]
  deriving (Eq, Show)

parseResource :: String -> Resource
parseResource (c:t) = read $ (toUpper c):t

parseReq :: [String] -> Resources
parseReq [n, r] = fromList [(parseResource r, read n)]

parseReqs :: [String] -> Resources
parseReqs reqs = add zero $ M.unions $ map parseReq $ splitOn ["and"] reqs

parseRobot :: [String] -> Plan
parseRobot ("Each":t:"robot":"costs":reqs) = Plan (parseResource t) $ parseReqs reqs
 
parseBlueprint :: [String] -> Blueprint
parseBlueprint [bps, rbts] = Blueprint i $ robotLst
  where i = read $ last $ words bps
        robotLst = map (parseRobot . words) $ splitOn ". " rbts

data State = State (Int, Resources, Resources, Maybe State)
-- data State = State (Int, Resources, Resources)
  deriving (Eq, Show, Ord)

add :: Resources -> Resources -> Resources
add = M.unionWith (+)

addX :: Resources -> Resource -> Resources
addX ra r = add ra (fromList [(r, 1)])

pay :: Resources -> Resources -> Resources
pay = M.unionWith (-)

has :: Resources -> Resources -> Bool
has inventory needed = all (>= 0) $ M.elems $ pay inventory needed

prefer :: Resource -> [Plan] -> [Plan]
prefer r plans = prefer' $ find ((== r) . kind) plans
  where prefer' Nothing = plans
        prefer' (Just a) = [a]

maxNeeded :: [Plan] -> Resources
maxNeeded = M.unionsWith max . map req

buildable :: Resources -> Plan -> Bool
buildable rsrc = has rsrc . req

newRobots :: [Plan] -> Resources -> [Plan]
-- newRobots plans = prefer Obsidian . prefer Geode . flip filter plans . buildable
newRobots plans = flip filter plans . buildable

nextStates :: [Plan] -> Int -> State -> [State]
nextStates plans totalTime state@(State (n, robots, rsrc, _)) | totalTime == n = []
                                                              | nrs == [] = noop
                                                              | otherwise = [State (n + 1, robots', rsrc', Just state) |
                                                                  (Plan k r) <- nrs,
                                                                  robots' <- [addX robots k],
                                                                  rsrc' <- [add robots (pay rsrc r)]] ++ noop
  where newR = newRobots plans
        mn = maxNeeded plans
        nrs = filter onlyNeeded $ newR rsrc
        noop = [State (n + 1, robots, add robots rsrc, Just state)]
        onlyNeeded (Plan k _) = k == Geode || (mn ! k - robots ! k) * (totalTime - n) > rsrc ! k
--         onlyNeeded (Plan k _) = k == Geode || (trace (show k ++ ": " ++ show (mn ! k) ++ " - " ++ (show (robots ! k)) ++ " * " ++ (show $ totalTime - n) ++ " > " ++ (show (rsrc ! k))) $ (mn ! k - robots ! k) * (totalTime - n) > rsrc ! k)

minute' :: [Plan] -> Int -> (Int -> [State] -> Int)
minute' plans totalTime = f
  where nxt = nextStates plans totalTime
        f maxGeodes [] = maxGeodes
        f maxGeodes (state@(State (n, robots, rsrc, _)):states) | n == totalTime = f (max maxGeodes $ countGeodes state) states
                                                                | otherwise = f maxGeodes $ nxt (stateTrace state) ++ states

minute :: [Plan] -> Int -> Int
minute plans totalTime = minutef 0 start
  where minutef = minute' plans totalTime

countGeodes :: State -> Int
countGeodes (State (_, _, r, _)) = r ! Geode
-- countGeodes (State (_, _, r)) = r ! Geode

solve :: Blueprint -> Int
solve (Blueprint id plans) = id * minute plans 24

start :: [State]
start = [State (0, addX zero Ore, zero, Nothing)]
-- start = [State (0, addX zero Ore, zero)]

main = do
  cnt <- getContents
--   print $ sum $ map (solve . parseBlueprint . splitOn ": ") $ lines cnt
  print $ head $ map (solve . parseBlueprint . splitOn ": ") $ lines cnt
