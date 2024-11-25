module Main where

import Debug.Trace
-- import Data.List
import Data.List.Split

import Text.PrettyPrint hiding (empty)
import qualified Text.PrettyPrint as T

idtrace x = trace (show x) x
infoTrace n str queue tiles tile image = trace (show ((text $ (show n ++ str)) $$
  (debugShow 3 $ reverse image) $$
  (text "\nqueue:") $$ (debugShow 12 queue) $$
  (text "\ntiles:") $$ (debugShow 12 tiles) $$
  (text "\ntile:") $$ (text $ show tile) $$
  (text "\n"))) image

debugShow es image = foldl ($$) T.empty $ map text $ join [""] $ map (map (join " ") . mapEach (map lineN [0..tmy])) $ chunksOf es image
  where (_, tmy) = addCoords (-1, -1) $ orientedSize $ head image

mapEach :: [a -> b] -> [a] -> [[b]]
mapEach = flip $ map . flip map

join _ [] = []
join j (s:ss) = foldl strBetween s ss
  where strBetween a b = a ++ j ++ b

data TileFile = Tile {tileId::Int, size::Coords, tileData::TileData, orientation::Orientation, flipped::Bool}

instance Show TileFile where
    show tile = show $ foldl ($$) (text "") $ map text $ ("Tile " ++ (show $ tileId tile) ++ ":"):showTileData tile

instance Eq TileFile where
    (==) a b = tileId a == tileId b

type TileData = [Char]

data Orientation = Up | Rgt | Dwn | Lft
                 deriving (Show, Eq, Ord, Enum)

orientations :: [Orientation]
orientations = enumFrom $ toEnum 0

type Direction = Orientation

type Coords = (Int, Int)

sizeX tile | orientation tile == Up = sx
           | orientation tile == Rgt = sy
           | orientation tile == Dwn = sx
           | orientation tile == Lft = sy
           where (sx, sy) = size tile

sizeY tile | orientation tile == Up = sy
           | orientation tile == Rgt = sx
           | orientation tile == Dwn = sy
           | orientation tile == Lft = sx
           where (sx, sy) = size tile

orientedSize tile = (sizeX tile, sizeY tile)

newTileFile = Tile 0 (0, 0) [] Up False

showTileData tile = chunksOf x $ map (loc tile) coords
  where coords = concat $ mapEach (map (flip (,)) [0..my]) [0..mx]
        (mx, my) = addCoords (-1, -1) (x, y)
        (x, y) = orientedSize tile

coordX (x, _) = x
coordY (_, y) = y

addCoords :: Coords -> Coords -> Coords
addCoords (ax, ay) (bx, by) = (ax + bx, ay + by)

next :: Orientation -> Orientation
next Rgt = Up
next o = succ o

rotate tile orientation = tile { orientation = orientation }

rotations tile = map (rotate tile) orientations

flipTile tile = tile { flipped = not $ flipped tile }

mutations tile = rs ++ map flipTile rs
  where rs = rotations tile

loc :: TileFile -> Coords -> Char
loc tile@(Tile _ _ _ _ True) (x, y) = loc tile { flipped = False } (x, my - y)
  where (_, my) = addCoords (-1, -1) $ orientedSize tile
loc (Tile _ (sx, sy) d Up False) (x, y) = d !! (x + y * sx)
loc (Tile _ (sx, sy) d Rgt False) (x, y) = d !! ((x + 1) * sx - (y + 1))
loc (Tile _ (sx, sy) d Dwn False) (x, y) = d !! ((sy - y) * sx - (x + 1))
loc (Tile _ (sx, sy) d Lft False) (x, y) = d !! ((sy - (x + 1)) * sx + y)

parseTileFile tile [] = tile:[]
parseTileFile tile ("":rst) = tile:parseTileFile newTileFile rst
parseTileFile tile (('T':'i':'l':'e':' ':idstr):rst) = parseTileFile (tile { tileId = read $ init idstr }) rst
parseTileFile tile (pixels:rst) = parseTileFile (tile { size = (length pixels, sizeY tile + 1), tileData = tileData tile ++ pixels }) rst

lineN :: Int -> TileFile -> TileData
lineN n tile = map (loc tile . (flip (,) n)) [0..mx]
  where (mx, _) = addCoords (-1, -1) $ orientedSize tile

edge :: TileFile -> Direction -> TileData
edge tile side | side == Up = map (loc tile . (flip (,) 0)) [0..mx]
               | side == Rgt = map (loc tile . ((,) mx)) [0..my]
               | side == Dwn = map (loc tile . (flip (,) my)) [0..mx]
               | side == Lft = map (loc tile . ((,) 0)) [0..my]
               where (mx, my) = addCoords (-1, -1) $ orientedSize tile

matchTile :: TileFile -> Direction -> TileFile -> Bool
matchTile ta Up tb = matchTile tb Dwn ta
matchTile ta Rgt tb = edge ta Rgt == edge tb Lft
matchTile ta Dwn tb = edge ta Dwn == edge tb Up
matchTile ta Lft tb = matchTile tb Rgt ta

mutateTileToMatch :: TileFile -> Direction -> TileFile -> [TileFile]
mutateTileToMatch ta dir tb = filter (matchTile ta dir) $ mutations tb

type Image = [TileFile]

edgeSize :: Image -> Int
edgeSize image = round $ sqrt $ fromIntegral $ length image

locTile :: Image -> Int -> Coords -> TileFile
locTile image es (x, y) = image !! (x + y * es)

showImage image = foldl (++) [] $ join [] $ map (map (join "") . mapEach (map lineN [0..tmy])) $ chunksOf es image
  where es = edgeSize image
        (_, tmy) = addCoords (-1, -1) $ orientedSize $ head image

findNextTiles tile dir tiles = concat $ map (mutateTileToMatch tile dir) $ filter ((/=) tile) tiles

findImageAlgorithm :: Int -> Int -> Image -> [[TileFile]] -> [TileFile] -> Image
findImageAlgorithm n es [] [] tiles = findImageAlgorithm (n+1) es [] [concat $ map mutations tiles] tiles
findImageAlgorithm n es [] ((q:queue):queues) tiles = findImageAlgorithm (n+1) es newImage (newQueue:oldQueue) tiles
                where newQueue = findNextTiles q Rgt tiles
                      oldQueue = queue:queues
                      newImage = [q]
--                       newImage = infoTrace n debugfmt newQueue tiles q [q]
                      debugfmt = " :a: (x, y) = " ++ show (x, y) ++ " | (nx, ny) = " ++ show (nx, ny)
                      doneLength = 0
                      (x, y) = (mod doneLength es, div doneLength es)
                      nextId = doneLength + 1
                      (nx, ny) = (mod nextId es, div nextId es)
findImageAlgorithm n es fixedImage@(_:prevImage) ([]:queues) tiles | doneLength == es * es = reverse fixedImage
                                                                   | otherwise = findImageAlgorithm (n+1) es newImage queues tiles
                where newImage = prevImage
--                 where newImage = infoTrace n debugfmt (head queues) [] newTileFile prevImage
                      debugfmt = " :b: (x, y) = " ++ show (x, y) ++ " | (nx, ny) = " ++ show (nx, ny)
                      doneLength = length fixedImage
                      (x, y) = (mod doneLength es, div doneLength es)
                      nextId = doneLength + 1
                      (nx, ny) = (mod nextId es, div nextId es)
findImageAlgorithm n es fixedImage@(prevTile:_) ((q:queue):queues) tiles = findImageAlgorithm (n+1) es newImage (newQueue:oldQueue) tiles
                where newQueue | x == es - 1 = findNextTiles nextUpTile Dwn unusedTiles
                               | y > 0 = filter (matchTile nextUpTile Dwn) $ findNextTiles q Rgt unusedTiles
                               | otherwise = findNextTiles q Rgt unusedTiles
                      debugfmt = " :c: (x, y) = " ++ show (x, y) ++ " | (nx, ny) = " ++ show (nx, ny)
                      oldQueue = queue:queues
                      unusedTiles = filter (not . flip elem newImage) tiles
                      newImage = q:fixedImage
--                       unusedTiles = filter (not . flip elem (q:fixedImage)) tiles
--                       newImage = infoTrace n debugfmt newQueue unusedTiles debugTile $ q:fixedImage
                      debugTile | x == es - 1 = nextUpTile
                                | y > 0 = nextUpTile
                                | otherwise = q
                      upTile = locTile (reverse fixedImage) es (x, y - 1)
                      nextUpTile = locTile (reverse fixedImage) es (nx, ny - 1)
                      doneLength = length fixedImage
                      (x, y) = (mod doneLength es, div doneLength es)
                      nextId = doneLength + 1
                      (nx, ny) = (mod nextId es, div nextId es)
findImageAlgorithm n es i q t = error $ show n ++ " / " ++ show es ++ " / " ++ show i ++ " / " ++ show q-- ++ " / " ++ show t

findImage :: [TileFile] -> Image
findImage tiles = findImageAlgorithm 0 (edgeSize tiles) [] [] tiles

removeEdges tile = newTileFile { size = (mx, my), tileData = map (loc tile) coords }
  where coords = concat $ mapEach (map (flip (,)) [1..my]) [1..mx]
        (mx, my) = addCoords (-2, -2) $ orientedSize tile

compileImage :: Image -> TileFile
compileImage image = newTileFile { size = (tx * es, ty * es), tileData = imageData }
  where imageData = showImage trimmedTiles
        trimmedTiles = map removeEdges image
        (tx, ty) = orientedSize $ head trimmedTiles 
        es = edgeSize image

monster = "                  # ":
          "#    ##    ##    ###":
          " #  #  #  #  #  #   ":[]

monsterCoords = filter (\(x, y) -> '#' == (monster !! y !! x)) coords
  where coords = concat $ mapEach (map (flip (,)) [0..my]) [0..mx]
        (mx, my) = addCoords (-1, -1) (w, h)
        w = length $ head monster
        h = length monster

matchMonsterChar ' ' _ = True
matchMonsterChar a b = a == b

matchMonster tdw = foldr (&&) True $ map (uncurry matchMonsterChar) $ zip (concat monster) (concat tdw)

tileWindow tile w h (x, y) = map (take w . drop x) $ take h $ drop y $ showTileData tile

findMonsters :: TileFile -> Bool
findMonsters tile = [] /= filter (matchMonster . tileWindow tile w h) coords
  where coords = concat $ mapEach (map (flip (,)) [0..my]) [0..mx]
        (mx, my) = addCoords (-w, -h) $ orientedSize tile
        w = length $ head monster
        h = length monster

setLoc :: Char -> TileFile -> Coords -> TileFile
setLoc c tile@(Tile _ _ _ _ True) (x, y) = tile { tileData = tileData $ setLoc c tile { flipped = False } (x, my - y) }
  where (_, my) = addCoords (-1, -1) $ orientedSize tile
setLoc c tile@(Tile _ (sx, sy) td dir False) (x, y) = tile { tileData = newtd }
  where i Up = (x + y * sx)
        i Rgt = ((x + 1) * sx - (y + 1))
        i Dwn = ((sy - y) * sx - (x + 1))
        i Lft = ((sy - (x + 1)) * sx + y)
        di = i dir
        newtd = take di td ++ c:(tail $ drop di td)

removeMonster :: TileFile -> Coords -> TileFile
removeMonster tile coords = foldl (setLoc '.') tile $ map (addCoords coords) monsterCoords

removeMonsters :: TileFile -> TileFile
removeMonsters tile = foldl removeMonster tile $ filter (matchMonster . tileWindow tile w h) coords
  where coords = concat $ mapEach (map (flip (,)) [0..my]) [0..mx]
        (mx, my) = addCoords (-w, -h) $ orientedSize tile
        w = length $ head monster
        h = length monster

countWaves :: TileFile -> Int
countWaves = length . filter (== '#') . tileData

checkHabitat tile = countWaves $ removeMonsters correctTile
  where correctTile = head $ filter findMonsters $ mutations tile

main = do
  cnt <- getContents
--   print $ parseTileFile newTileFile $ lines cnt
--   print $ showImage $ parseTileFile newTileFile $ lines cnt
--   print $ showImage $ findImage $ parseTileFile newTileFile $ lines cnt
--   print $ compileImage $ findImage $ parseTileFile newTileFile $ lines cnt
  print $ checkHabitat $ compileImage $ findImage $ parseTileFile newTileFile $ lines cnt

