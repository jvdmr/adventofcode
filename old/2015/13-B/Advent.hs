module Main where

import Data.List hiding (insert)
import Data.Map (insert, (!), keys)
import qualified Data.Map as M
import Text.Parsec

import Debug.Trace
idtrace x = trace (show x) x
counttrace x = trace (show count ++ " <== " ++ show x) x
  where count = foldl (+) 0 $ map fst x

uniq :: Eq a => [a] -> [a]
uniq (a:b:rst) | a == b = uniq (b:rst)
               | otherwise = a:uniq (b:rst)
uniq a = a

data Name = Name String
  deriving (Eq, Ord, Show)

data DirectionalHappiness = DI Name Int Name
  deriving (Eq, Show)

instance Ord DirectionalHappiness where
  compare (DI _ a _) (DI _ b _) = compare a b

name =     many1 letter >>= return . Name
       <?> "name"

happinessAmount =     try (string "lose " >> many1 digit >>= return . (\n -> (- n)) . read)
                  <|> (string "gain " >> many1 digit >>= return . read)
                  <?> "happinessAmount"

directionalHappiness =     name >>= \a -> string " would " >> happinessAmount >>= \n -> string " happiness units by sitting next to " >> name >>= return . DI a n
                       <?> "directionalHappiness"

sentence =     directionalHappiness >>= \di -> char '.' >> return di
           <?> "sentence"

parseString :: String -> DirectionalHappiness
parseString = right . parse sentence "(source)"
  where right (Right ast) = ast
        right (Left a) = error $ show a

getSelf :: DirectionalHappiness -> Name
getSelf (DI self _ _) = self

addMyself :: [DirectionalHappiness] -> [DirectionalHappiness]
addMyself others = others ++ meAndOthers ++ othersAndMe
  where meAndOthers = map (DI (Name "me") 0) $ uniq $ sort $ map getSelf others
        othersAndMe = map flipHappiness meAndOthers
        flipHappiness (DI a n b) = DI b n a

simplifyTotalHappiness :: [DirectionalHappiness] -> [DirectionalHappiness]
simplifyTotalHappiness dhs = map calculateTotalHappiness dhs
  where calculateTotalHappiness (DI a h b) = DI a (h + (happiness ! (b, a))) b
        happiness = M.fromList $ map asHappinessPair dhs
        asHappinessPair (DI x n y) = ((x, y), n)

type Happiness = [(Int, Name)]

add :: Name -> Int -> Happiness -> Happiness
add other happiness others = (happiness, other):others

type Graph = M.Map Name Happiness

(?!) :: Graph -> Name -> Happiness
(?!) g name = M.findWithDefault [] name g

createGraph :: Graph -> [DirectionalHappiness] -> Graph
createGraph g [] = g
createGraph g ((DI a h b):rst) = createGraph (insert a (add b h (g ?! a)) g) rst

doTsp :: Graph -> [Name] -> Name -> [Happiness]
doTsp g visited n | nextNodes == [] = [[head $ filter ((== (last visited)) . snd) (g ! n)]]
                  | otherwise = concat $ map (\next -> map (next:) (doTsp g (n:visited) $ snd next)) nextNodes
                  where nextNodes = filter (not . flip elem visited . snd) $ (g ! n)

tsp :: Graph -> [Happiness]
tsp g = concat $ map (doTsp g []) ks
  where ks = keys g

main = do
  cnt <- getContents
  print $ head $ reverse $ sort $ map (foldl (+) 0 . map fst . counttrace) $ tsp $ createGraph M.empty $ addMyself $ simplifyTotalHappiness $ map parseString $ lines cnt

