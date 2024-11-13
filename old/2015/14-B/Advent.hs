module Main where

import Data.List hiding (insert)
import Data.Map (insert, (!), keys)
import qualified Data.Map as M
import Text.Parsec

data Name = Name String
  deriving (Eq, Ord, Show)

data Reindeer = Deer Name Int Int Int
  deriving (Eq, Show)

name =     many1 letter >>= return . Name
       <?> "name"

time =     string " for " >> many1 digit >>= \a -> string " seconds" >> return (read a)
       <?> "time"

speed =     many1 digit >>= \a -> string " km/s" >> return (read a)
        <?> "time"

deer =     name >>= \a -> string " can fly " >> speed >>= \s -> time >>= \d -> string ", but then must rest" >> time >>= return . Deer a s d
       <?> "deer"

sentence =     deer >>= \d -> char '.' >> return d
           <?> "sentence"

parseString :: String -> Reindeer
parseString = right . parse sentence "(source)"
  where right (Right ast) = ast
        right (Left a) = error $ show a

data DeerPos = DeerPos Name Int
  deriving (Eq, Show)

instance Ord DeerPos where
  compare (DeerPos _ a) (DeerPos _ b) = compare a b

getName (DeerPos n _) = n

position secs (Deer n s t r) = DeerPos n $ s * (min t (secs `mod` (t + r))) + (secs `div` (t + r)) * s * t

filterLeaders [a] = [a]
filterLeaders (a@(DeerPos _ p1):b@(DeerPos _ p2):rst) | p1 == p2 = a:filterLeaders (b:rst)
                                                      | otherwise = [a]

race secs deer = map (\f -> filterLeaders $ reverse $ sort $ map f deer) $ map position [1..secs]

countLeaders :: Eq a => Int -> [a] -> [Int]
countLeaders n [_] = [n]
countLeaders n (a:b:rst) | a == b = countLeaders (n + 1) (b:rst)
                         | otherwise = n:countLeaders 1 (b:rst)

-- racetime = 1000
racetime = 2503

main = do
  cnt <- getContents
  print $ head $ reverse $ sort $ countLeaders 1 $ sort $ map getName $ concat $ race racetime $ map parseString $ lines cnt

