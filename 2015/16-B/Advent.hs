module Main where

import Data.List hiding (insert)
import Text.Parsec
import Data.Functor.Identity

import Debug.Trace
idtrace x = trace (show x) x

flatten :: [[a]] -> [a]
flatten [] = []
flatten (a:rst) = a ++ flatten rst

data Compound = Compound String Int
  deriving (Eq, Show)

data Sue = Sue Int [Compound]
  deriving (Eq, Show)

sue =     string "Sue " >> many1 digit >>= return . Sue . read
      <?> "sue"

compoundname :: ParsecT String u Data.Functor.Identity.Identity (Int -> Compound)
compoundname =     oneOf ":," >> space >> many1 letter >>= return . Compound
               <?> "compoundname"

value :: (Int -> Compound) -> ParsecT String u Data.Functor.Identity.Identity Compound
value partialCompound =     string ": " >> many1 digit >>= return . partialCompound . read
                        <?> "value"

compound =     compoundname >>= value >>= return
           <?> "compound"

compounds partialSue =     many1 compound >>= return . partialSue
                       <?> "compounds"

auntsue =     sue >>= compounds >>= return
          <?> "auntsue"

parseString :: String -> Sue
parseString = right . parse auntsue "(source)"
  where right (Right ast) = ast
        right (Left a) = error $ show a

matchCompound [] _ = False
matchCompound (c@(Compound "cats" rd):cs) somec@(Compound "cats" n) = rd < n || matchCompound cs somec
matchCompound (c@(Compound "trees" rd):cs) somec@(Compound "trees" n) = rd < n || matchCompound cs somec
matchCompound (c@(Compound "pomeranians" rd):cs) somec@(Compound "pomeranians" n) = rd > n || matchCompound cs somec
matchCompound (c@(Compound "goldfish" rd):cs) somec@(Compound "goldfish" n) = rd > n || matchCompound cs somec
matchCompound (c:cs) somec = c == somec || matchCompound cs somec

mfcsam (Sue _ somecs) = foldl (&&) True $ map (matchCompound cs) somecs
  where (Sue _ cs) = parseString "Sue 0: children: 3, cats: 7, samoyeds: 2, pomeranians: 3, akitas: 0, vizslas: 0, goldfish: 5, trees: 3, cars: 2, perfumes: 1"

main = do
  cnt <- getContents
  print $ filter mfcsam $ map parseString $ lines cnt

