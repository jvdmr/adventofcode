module Main where

import Data.List hiding (insert, empty)
import Data.List.Split
import Data.Map.Strict ((!), insert, empty, member, Map, elems, size)
import qualified Data.Map.Strict as M

data Food = Food {ingredients::[String], allergens::[String]}
  deriving (Show, Eq, Ord)

parseFood line = Food (words a) (splitOn ", " $ init b)
  where (a:b:[]) = splitOn " (contains " line

foodAllergens (Food i a) = map ((,) i) a

reduce mem [] = mem
reduce mem ((is, a):foods) | member a mem = reduce (insert a (filter (flip elem is) $ mem ! a) mem) foods
                           | otherwise = reduce (insert a is mem) foods

filterField :: Map k [String] -> String -> Map k [String]
filterField mem ingr = M.map removeUnlessSingleton mem
  where removeUnlessSingleton ingrs | length ingrs == 1 = ingrs
                                    | otherwise = filter (/= ingr) ingrs

getSingletons :: Map String [String] -> [String]
getSingletons = concat . elems . M.filter ((== 1) . length)

join _ [] = []
join j (s:ss) = foldl strBetween s ss
  where strBetween a b = a ++ j ++ b

furtherReduce mem | size (M.filter ((> 1) . length) mem) == 0 = join "," $ getSingletons mem
                  | otherwise = furtherReduce nextStep
                      where nextStep = foldl filterField mem singletons
                            singletons = getSingletons mem

unsafeIngredients foods = furtherReduce allergensIn
  where suspectIngredients = nub $ sort $ M.foldl (++) [] $ allergensIn
        allergensIn = reduce empty $ concat $ map foodAllergens foods
        allIngredients = foldl (++) [] $ map ingredients foods
        safeIngredients = filter (not . flip elem suspectIngredients) allIngredients

main = do
  cnt <- getContents
  print $ unsafeIngredients $ map parseFood $ lines cnt

