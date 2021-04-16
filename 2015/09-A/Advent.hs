module Main where

import Data.List hiding (insert)
import Data.Map (insert, (!), keys)
import qualified Data.Map as M
import Text.Parsec

import Debug.Trace
idtrace x = trace (show x) x

type Graph = M.Map City [Distance]

data City = City String
            deriving (Eq, Ord)

instance Show City where
  show (City a) = a

data Distance = Distance (City, City) Int
                deriving (Eq)

instance Show Distance where
  show (Distance (a, b) dist) = show a ++ " to " ++ show b ++ " = " ++ show dist

instance Ord Distance where
  compare (Distance _ distA) (Distance _ distB) = compare distA distB

city =     many1 letter >>= return . City
       <?> "city"

distance =     city >>= \a -> string " to " >> city >>= \b -> string " = " >> many1 digit >>= return . Distance (a, b) . read
           <?> "distance"

parseString :: String -> Distance
parseString = right . parse distance "(source)"
  where right (Right ast) = ast
        right (Left a) = error $ show a

getDistance :: Distance -> Int
getDistance (Distance _ d) = d

getDestination :: Distance -> City
getDestination (Distance (_, b) _) = b

reverseDistance :: Distance -> Distance
reverseDistance (Distance (a, b) d) = Distance (b, a) d

(?!) :: Graph -> City -> [Distance]
(?!) g k = M.findWithDefault [] k g

createGraph :: Graph -> [Distance] -> Graph
createGraph g [] = g
createGraph g (dist@(Distance (a, b) d):rst) = createGraph (insert a (dist:(g ?! a)) (insert b ((reverseDistance dist):(g ?! b)) g)) rst

doTsp :: Graph -> [City] -> City -> [Distance]
doTsp g visited n | nextNodes == [] = []
                  | otherwise = next:(doTsp g (n:visited) nextCity)
                  where nextNodes = sort $ filter (not . flip elem visited . getDestination) $ (g ! n)
                        next = head nextNodes
                        nextCity = getDestination next

tsp :: Graph -> [[Distance]]
tsp g = filter ((==) (length ks) . ((+) 1) . length . idtrace) $ map (doTsp g []) ks
  where ks = keys g

main = do
  cnt <- getContents
  print $ head $ sort $ map (foldl (+) 0 . map getDistance) $ tsp $ createGraph M.empty $ idtrace $ map parseString $ lines cnt

