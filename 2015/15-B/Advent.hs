module Main where

import Data.List hiding (insert)
import Text.Parsec

import Debug.Trace
idtrace x = trace (show x) x

flatten :: [[a]] -> [a]
flatten [] = []
flatten (a:rst) = a ++ flatten rst

data Ingredient = Ingredient String Int Int Int Int Int
  deriving (Eq, Show)

name =     many1 letter >>= return . Ingredient
       <?> "name"

value =     try (char '-' >> many1 digit >>= return . negate . read)
        <|> (many1 digit >>= return . read)
        <?> "value"

capacity partialIngredient =     string ": capacity " >> value >>= return . partialIngredient
                             <?> "capacity"

durability partialIngredient =     string ", durability " >> value >>= return . partialIngredient
                               <?> "durability"

flavor partialIngredient =     string ", flavor " >> value >>= return . partialIngredient
                           <?> "flavor"

texture partialIngredient =     string ", texture " >> value >>= return . partialIngredient
                            <?> "texture"

calories partialIngredient =     string ", calories " >> value >>= return . partialIngredient
                             <?> "calories"

ingredient =     name >>= capacity >>= durability >>= flavor >>= texture >>= calories >>= return
             <?> "ingredient"

parseString :: String -> Ingredient
parseString = right . parse ingredient "(source)"
  where right (Right ast) = ast
        right (Left a) = error $ show a

instance Num Ingredient where
  (+) (Ingredient n a b c d e) (Ingredient m v w x y z) = Ingredient (n ++ " + " ++ m) (a + v) (b + w) (c + x) (d + y) (e + z)
  (*) (Ingredient n a b c d e) (Ingredient m v w x y z) = Ingredient (n ++ " * " ++ m) (a * v) (b * w) (c * x) (d * y) (e * z)
  negate (Ingredient n a b c d e) = Ingredient n (negate a) (negate b) (negate c) (negate d) (negate e)

-- score i@(Ingredient _ a b c d _) = trace (show i ++ " -> " ++ show total) total
score i@(Ingredient _ a b c d _) = total
  where total = a * b * c * d

spoons (Ingredient n a b c d e) s = Ingredient (n ++ " * " ++ show s) (s * a) (s * b) (s * c) (s * d) (s * e)

neutral = Ingredient "nothing" 0 0 0 0 0

nullify (Ingredient n a b c d e) = Ingredient n (max 0 a) (max 0 b) (max 0 c) (max 0 d) (max 0 e)

rangify 1 m = map (:[]) [1..m]
rangify n m = flatten $ map (\a -> map (\b -> b:a) r) $ rangify (n - 1) m
  where r = [1..m]

combine m ings = map (foldl (+) neutral . zipWith spoons ings) $ filter ((==) m . foldl (+) 0) $ rangify (length ings) m

diet (Ingredient _ _ _ _ _ 500) = True
diet _ = False

main = do
  cnt <- getContents
  print $ head $ reverse $ sort $ map (score . nullify) $ filter diet $ combine 100 $ map parseString $ lines cnt

