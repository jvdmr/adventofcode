module Main where

import Data.List hiding (empty, insert)
import Data.Map (empty, (!), insert, Map, member, elems)

import Text.Parsec
import Text.PrettyPrint (($$), text)

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

main = do
  cnt <- getContents
  print $ foldl flipTile empty $ lines cnt
  print $ length $ filter (== Black) $ elems $ foldl flipTile empty $ lines cnt

