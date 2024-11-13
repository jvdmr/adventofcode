module Main where

import Data.List hiding (insert, empty)
import Data.List.Split
import Data.Map.Strict ((!), insert, empty, member)
import qualified Data.Map.Strict as M

data Food = Food {ingredients::[String], allergens::[String]}
  deriving (Show, Eq, Ord)

parseFood line = Food (words a) (splitOn ", " $ init b)
  where (a:b:[]) = splitOn " (contains " line

foodAllergens (Food i a) = map ((,) i) a

reduce mem [] = mem
reduce mem ((is, a):foods) | member a mem = reduce (insert a (filter (flip elem is) $ mem ! a) mem) foods
                           | otherwise = reduce (insert a is mem) foods


findSafeIngredients foods = filter (not . flip elem suspectIngredients) allIngredients
  where suspectIngredients = nub $ sort $ M.foldl (++) [] $ reduce empty $ concat $ map foodAllergens foods
        allIngredients = foldl (++) [] $ map ingredients foods

main = do
  cnt <- getContents
  print $ length $ findSafeIngredients $ map parseFood $ lines cnt

