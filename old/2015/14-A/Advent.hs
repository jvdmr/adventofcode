module Main where

import Data.List hiding (insert)
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

position secs (Deer _ s t r) = s * (min t (secs `mod` (t + r))) + (secs `div` (t + r)) * s * t

-- racetime = 1000
racetime = 2503

main = do
  cnt <- getContents
  print $ head $ reverse $ sort $ map (position racetime . parseString) $ lines cnt

