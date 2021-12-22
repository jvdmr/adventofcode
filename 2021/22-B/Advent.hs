module Main where

import Data.List
import Data.List.Split

import Debug.Trace
idtrace x = trace (show x) x
ftrace f x = trace (show $ f x) x

flatten :: [[a]] -> [a]
flatten = foldl (++) []

data Instruction = On (Int, Int) (Int, Int) (Int, Int)
                 | Off (Int, Int) (Int, Int) (Int, Int)
                 deriving (Show, Eq)

apply :: (a -> a -> b) -> [a] -> b
apply f ls = f (head ls) (last ls)

range :: Int -> Int -> [Int]
range a b = [a..b]

make :: (f a -> f a -> f a -> Instruction) -> [f a] -> Instruction
make ins ranges = ins (ranges !! 0) (ranges !! 1) (ranges !! 2)

rangesToInts :: (Int -> Int -> f Int) -> String -> [f Int]
rangesToInts f = map (apply f . map read . splitOn ".." . tail . tail) . splitOn ","

parseStep :: String -> Instruction
parseStep ('o':'n':' ':ranges) = make On $ rangesToInts (,) ranges
parseStep ('o':'f':'f':' ':ranges) = make Off $ rangesToInts (,) ranges

data CubeRange = Empty
               | Cubes {xr::(Int, Int), yr::(Int, Int), zr::(Int, Int)}
               deriving (Show, Eq)

countCubes :: CubeRange -> Int
countCubes Empty = 0
countCubes (Cubes (xa, xb) (ya, yb) (za, zb)) = (xb - xa + 1) * (yb - ya + 1) * (zb - za + 1)

notEmpty :: CubeRange -> Bool
notEmpty Empty = False
notEmpty _ = True

fromInstruction :: Instruction -> CubeRange
fromInstruction (On xs ys zs) = Cubes xs ys zs
fromInstruction (Off xs ys zs) = Cubes xs ys zs

flipInstruction :: Instruction -> Instruction
flipInstruction (On xs ys zs) = Off xs ys zs

cuberange :: (Int, Int) -> (Int, Int) -> (Int, Int) -> CubeRange
cuberange (xa, xb) (ya, yb) (za, zb) | xa > xb || ya > yb || za > zb = Empty
                                     | otherwise = Cubes (xa, xb) (ya, yb) (za, zb)

cutCubeRange :: Instruction -> CubeRange -> [CubeRange]
cutCubeRange (Off (xoa, xob) (yoa, yob) (zoa, zob)) c@(Cubes (xa, xb) (ya, yb) (za, zb)) | xoa > xb || xob < xa || yoa > yb || yob < ya || zoa > zb || zob < za = [c]
                                                                                         | otherwise = [cuberange (xa, xoa - 1) (ya, yb) (za, zb),
                                                                                                        cuberange (max xa xoa, min xb xob) (ya, yoa - 1) (za, zb),
                                                                                                        cuberange (max xa xoa, min xb xob) (max ya yoa, min yb yob) (za, zoa - 1),
                                                                                                        cuberange (max xa xoa, min xb xob) (max ya yoa, min yb yob) (zob + 1, zb),
                                                                                                        cuberange (max xa xoa, min xb xob) (yob + 1, yb) (za, zb),
                                                                                                        cuberange (xob + 1, xb) (ya, yb) (za, zb)]

switchCubes :: [CubeRange] -> Instruction -> [CubeRange]
switchCubes cubes ins@(On _ _ _) = fromInstruction ins:switchCubes cubes (flipInstruction ins)
switchCubes cubes ins@(Off _ _ _) = flatten $ map (filter notEmpty . cutCubeRange ins) cubes

main = do
  cnt <- getContents
--   print $ foldl (+) 0 $ map countCubes $ foldl (switchCubes . ftrace (map countCubes) . idtrace) [] $ map idtrace $ map parseStep $ lines cnt
  print $ foldl (+) 0 $ map countCubes $ foldl switchCubes [] $ map parseStep $ lines cnt

