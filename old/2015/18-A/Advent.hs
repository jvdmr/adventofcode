module Main where

import Data.List hiding (empty, insert)
import Text.PrettyPrint

between :: Int -> Int -> Int -> Bool
between a b c = a <= b && b <= c

mapEach :: [a -> b] -> [a] -> [[b]]
mapEach = flip $ map . flip map

mapArea :: (a -> b) -> [[a]] -> [[b]]
mapArea f a = map (map f) a

data State = On | Off
  deriving (Eq)

instance Show State where
  show On = "#"
  show Off = "."

type Grid = [[State]]

showGrid :: Grid -> Doc
showGrid g = foldl ($$) (text "") $ map (text . foldl (++) "" . map show) g

type Coords = (Int, Int)

cx :: Coords -> Int
cx (x, _) = x

cy :: Coords -> Int
cy (_, y) = y

addCoords :: Coords -> Coords -> Coords
addCoords (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

lit :: State -> Bool
lit On = True
lit Off = False

sizeX :: Grid -> Int
sizeX g = length g

sizeY :: Grid -> Int
sizeY g = length $ head g

getLight :: Grid -> Coords -> State
getLight g (x, y) | x < 0 || y < 0 || x >= sizeX g || y >= sizeY g = Off
                  | otherwise = head $ drop y $ head $ drop x g

parseState :: Char -> State
parseState '#' = On
parseState '.' = Off

parseLine :: [Char] -> [State]
parseLine = map parseState

parseGrid :: [[Char]] -> Grid
parseGrid = map parseLine

areaCoords :: [Int] -> [Int] -> [[Coords]]
areaCoords xs ys = mapEach (map (,) xs) ys

neighbors :: Coords -> [Coords]
neighbors c@(x, y) = filter (not . (==) c) $ map (addCoords c) $ concat $ areaCoords range range
  where range = [-1, 0, 1]

lifeLight :: Grid -> Coords -> State
lifeLight g c | lightOn && between 2 neighborCount 3 = On
              | neighborCount == 3 = On
              | otherwise = Off
              where lightOn = lit $ getLight g c
                    neighborCount = length $ filter lit $ map (getLight g) $ neighbors c

life :: Grid -> Grid
life g = mapArea (lifeLight g) $ areaCoords [0..maxX] [0..maxY]
  where maxX = (sizeX g) - 1
        maxY = (sizeY g) - 1

lifeSteps :: Int -> Grid -> Grid
lifeSteps n g = head $ drop n $ iterate life g

countLights :: Grid -> Int
countLights = length . filter lit . concat

main = do
  cnt <- getContents
  print $ showGrid $ parseGrid $ lines cnt
  print $ showGrid $ lifeSteps 1 $ parseGrid $ lines cnt
  print $ showGrid $ lifeSteps 2 $ parseGrid $ lines cnt
  print $ showGrid $ lifeSteps 3 $ parseGrid $ lines cnt
  print $ showGrid $ lifeSteps 4 $ parseGrid $ lines cnt
  print $ showGrid $ lifeSteps 100 $ parseGrid $ lines cnt
--   print $ map showGrid $ take 100 $ iterate life $ parseGrid $ lines cnt
  print $ countLights $ lifeSteps 100 $ parseGrid $ lines cnt
