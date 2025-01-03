module Main where

-- import Data.List
import Data.List.Split

infList x = x:infList x

data State = On | Off | Toggle

type Grid = [[Int]]

type Coords = (Int, Int)

type CoordRange = (Coords, Coords)

cx (x, _) = x
cy (_, y) = y

fromx (ca, _) = cx ca
fromy (ca, _) = cy ca
tox (_, cb) = cx cb
toy (_, cb) = cy cb
rx range = tox range - fromx range + 1
ry range = toy range - fromy range + 1

toggle i = i + 2
on i = i + 1
off 0 = 0
off i = i - 1

setState On lst = map on lst
setState Off lst = map off lst
setState Toggle lst = map toggle lst

setRange g state range = (take (fromx range) g) ++ (map (\row -> (take (fromy range) row) ++ (setState state $ take (ry range) $ drop (fromy range) row) ++ (drop (1 + toy range) row)) $ take (rx range) $ drop (fromx range) g) ++ (drop (1 + tox range) g)

gridSize = 1000
_lightRow = take gridSize $ infList 0
lightGrid :: Grid
lightGrid = take gridSize $ infList _lightRow

parseRange rangestr = ((ax, ay), (bx, by))
  where [[ax, ay], [bx, by]] = map (map read . splitOn ",") $ splitOn " through " rangestr

execLight g ('t':'u':'r':'n':' ':'o':'n':' ':rangestr) = setRange g On $ parseRange rangestr
execLight g ('t':'u':'r':'n':' ':'o':'f':'f':' ':rangestr) = setRange g Off $ parseRange rangestr
execLight g ('t':'o':'g':'g':'l':'e':' ':rangestr) = setRange g Toggle $ parseRange rangestr

main = do
  cnt <- getContents
  print $ foldl (+) 0 $ concat $ foldl execLight lightGrid $ lines cnt

