module Main where

import Data.List hiding (empty, insert)
import Data.Map (empty, (!), insert, Map, member, elems, findWithDefault, fromList, keys)
import qualified Data.Map as M

import Text.Parsec hiding (between)
import Text.PrettyPrint (($$), text)

flatten :: [[a]] -> [a]
flatten = foldl (++) []

mapEach :: [a -> b] -> [a] -> [[b]]
mapEach = flip $ map . flip map

data Direction = NE | E | SE | SW | W | NW
  deriving (Show, Eq, Ord, Enum)

data Color = White | Black
  deriving (Show, Eq, Ord, Enum)

switchColor Black = White
switchColor White = Black

type Coords = (Int, Int)

dir =     try (char 'n' >> char 'e' >> return NE)
      <|> (char 'n' >> char 'w' >> return NW)
      <|> try (char 's' >> char 'e' >> return SE)
      <|> (char 's' >> char 'w' >> return SW)
      <|> (char 'e' >> return E)
      <|> (char 'w' >> return W)
      <?> "dir"

tile =     many1 dir >>= return
       <?> "tile"

parseTile :: String -> [Direction]
parseTile s = right $ parse tile s s
  where right (Right t) = t
        right (Left t) = error $ show t

flipTile :: Map Coords Color -> String -> Map Coords Color
flipTile m s = insert tile flippedTile m
  where flippedTile | member tile m = switchColor $ m ! tile
                    | otherwise = Black
        tile = toCoords (0, 0) $ parseTile s

toCoords :: Coords -> [Direction] -> Coords
toCoords c [] = c
toCoords (x, y) (E:rst) = toCoords (x + 1, y) rst
toCoords (x, y) (NE:rst) = toCoords (x, y + 1) rst
toCoords (x, y) (SE:rst) = toCoords (x + 1, y - 1) rst
toCoords (x, y) (W:rst) = toCoords (x - 1, y) rst
toCoords (x, y) (NW:rst) = toCoords (x - 1, y + 1) rst
toCoords (x, y) (SW:rst) = toCoords (x, y - 1) rst

addCoords (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

surroundings m c = filter id $ map (flip member m . addCoords c) [(1, 0), (1, -1), (0, -1), (-1, 0), (-1, 1), (0, 1)]

between a b c = a <= b && b <= c

life m c | t == Black && between 1 bc 2 = Black
         | t == White && bc /= 2 = White
         | otherwise = switchColor t
         where t = findWithDefault White c m
               bc = length $ surroundings m c

allCoords coords = flatten $ flip mapEach [-my .. my] $ map (,) [-mx .. mx]
  where maxC :: Coords -> Coords -> Coords
        maxC (x1, y1) (x2, y2) = (max (abs x1) (abs x2), max (abs y1) (abs y2))
        (mx, my) = addCoords (1, 1) $ foldl maxC (0, 0) coords

step m = fromList $ filter ((== Black) . snd) $ map f $ allCoords $ keys m
  where f c = (c, life m c)

printFloor m = foldl ($$) (text "") $ map (text . printRow . map strdat) $ flip mapEach [-mx .. mx] $ map (flip (,)) $ reverse [-my .. my]
  where (mx, my) = minC (10, 10) $ foldl maxC (0, 0) coords
        maxC (x1, y1) (x2, y2) = (max (abs x1) (abs x2), max (abs y1) (abs y2))
        minC (x1, y1) (x2, y2) = (min (abs x1) (abs x2), min (abs y1) (abs y2))
        coords = keys m
        strdat (x, y) = ((x + mx, y + my), findWithDefault White (x, y) m)
        printRow tiles@(((_, y), _):_) = (show $ my - y) ++ (if (my - y < 0) then "" else " ") ++ "  " ++ (padding y) ++ (foldl (++) "" $ map (color . snd) tiles) ++ padding (2 * my - y)
        printRow a = error $ show a ++ " <<>> " ++ show m
        color White = "##"
        color Black = "  "
        padding 0 = []
        padding y = '#':padding (y - 1)

main = do
  cnt <- getContents
  print $ foldl flipTile empty $ lines cnt
  print $ printFloor $ foldl flipTile empty $ lines cnt
  print $ length $ elems $ M.filter (== Black) $ foldl flipTile empty $ lines cnt
  print $ printFloor $ (iterate step $ M.filter (== Black) $ foldl flipTile empty $ lines cnt) !! 1
  print $ length $ elems $ (iterate step $ M.filter (== Black) $ foldl flipTile empty $ lines cnt) !! 1
  print $ printFloor $ (iterate step $ M.filter (== Black) $ foldl flipTile empty $ lines cnt) !! 2
  print $ length $ elems $ (iterate step $ M.filter (== Black) $ foldl flipTile empty $ lines cnt) !! 2
  print $ printFloor $ (iterate step $ M.filter (== Black) $ foldl flipTile empty $ lines cnt) !! 3
  print $ length $ elems $ (iterate step $ M.filter (== Black) $ foldl flipTile empty $ lines cnt) !! 3
  print $ printFloor $ (iterate step $ M.filter (== Black) $ foldl flipTile empty $ lines cnt) !! 4
  print $ length $ elems $ (iterate step $ M.filter (== Black) $ foldl flipTile empty $ lines cnt) !! 4
  print $ printFloor $ (iterate step $ M.filter (== Black) $ foldl flipTile empty $ lines cnt) !! 5
  print $ length $ elems $ (iterate step $ M.filter (== Black) $ foldl flipTile empty $ lines cnt) !! 5
  print $ printFloor $ (iterate step $ M.filter (== Black) $ foldl flipTile empty $ lines cnt) !! 6
  print $ length $ elems $ (iterate step $ M.filter (== Black) $ foldl flipTile empty $ lines cnt) !! 6
  print $ printFloor $ (iterate step $ M.filter (== Black) $ foldl flipTile empty $ lines cnt) !! 7
  print $ length $ elems $ (iterate step $ M.filter (== Black) $ foldl flipTile empty $ lines cnt) !! 7
  print $ printFloor $ (iterate step $ M.filter (== Black) $ foldl flipTile empty $ lines cnt) !! 8
  print $ length $ elems $ (iterate step $ M.filter (== Black) $ foldl flipTile empty $ lines cnt) !! 8
  print $ printFloor $ (iterate step $ M.filter (== Black) $ foldl flipTile empty $ lines cnt) !! 9
  print $ length $ elems $ (iterate step $ M.filter (== Black) $ foldl flipTile empty $ lines cnt) !! 9
  print $ printFloor $ (iterate step $ M.filter (== Black) $ foldl flipTile empty $ lines cnt) !! 10
  print $ length $ elems $ (iterate step $ M.filter (== Black) $ foldl flipTile empty $ lines cnt) !! 10
  print $ length $ elems $ (iterate step $ M.filter (== Black) $ foldl flipTile empty $ lines cnt) !! 20
  print $ length $ elems $ (iterate step $ M.filter (== Black) $ foldl flipTile empty $ lines cnt) !! 30
  print $ length $ elems $ (iterate step $ M.filter (== Black) $ foldl flipTile empty $ lines cnt) !! 40
  print $ length $ elems $ (iterate step $ M.filter (== Black) $ foldl flipTile empty $ lines cnt) !! 50
  print $ length $ elems $ (iterate step $ M.filter (== Black) $ foldl flipTile empty $ lines cnt) !! 60
  print $ length $ elems $ (iterate step $ M.filter (== Black) $ foldl flipTile empty $ lines cnt) !! 70
  print $ length $ elems $ (iterate step $ M.filter (== Black) $ foldl flipTile empty $ lines cnt) !! 80
  print $ length $ elems $ (iterate step $ M.filter (== Black) $ foldl flipTile empty $ lines cnt) !! 90
  print $ length $ elems $ (iterate step $ M.filter (== Black) $ foldl flipTile empty $ lines cnt) !! 100 

