module Main where

import Data.List
import Text.PrettyPrint

between :: Int -> Int -> Int -> Bool
between a b c = a <= b && b <= c

mapEach :: [a -> b] -> [a] -> [[b]]
mapEach = flip $ map . flip map

type Cube = Char
type Space = [[[[Cube]]]]
type Coords = (Int, Int, Int, Int)

coords x y z w = (x, y, z, w)

add :: Coords -> Coords -> Coords
add (x1, y1, z1, w1) (x2, y2, z2, w2) = (x1+x2, y1+y2, z1+z2, w1+w2)

origin :: Coords
origin = coords 0 0 0 0

coordToIndex (x, y, z, w) = (c2i x, c2i y, c2i z, c2i w)
  where c2i a | a < 0 = (a * (- 2)) - 1
              | otherwise = a * 2

indexToCoord (xi, yi, zi, wi) = (i2c xi, i2c yi, i2c zi, i2c wi)
  where i2c a | even a = div a 2
              | otherwise = div (a + 1) (-2)

size :: Space -> (Coords, Coords)
size space = ((div x (-2), div y (-2), div z (-2), div w (-2)), (div x 2, div y 2, div z 2, div w 2))
  where (x, y, z, w) = add (-1, -1, -1, -1) $ actualSize space

actualSize :: Space -> (Int, Int, Int, Int)
actualSize space = (length $ head $ head $ head space, length $ head $ head space, length $ head space, length space)

within :: Space -> Coords -> Bool
within space (x, y, z, w) = withinX && withinY && withinZ && withinW
  where ((ax, ay, az, aw), (bx, by, bz, bw)) = size space
        withinX = between ax x bx
        withinY = between ay y by
        withinZ = between az z bz
        withinW = between aw w bw

loc :: Space -> Coords -> Cube
loc space pos | within space pos = head $ drop xi $ head $ drop yi $ head $ drop zi $ head $ drop wi space
              | otherwise = '.'
  where (xi, yi, zi, wi) = coordToIndex pos

surroundSpace :: Space -> Space
surroundSpace space = (map (++ [newZ, newZ]) $ map (map (++ [newY, newY])) $ map (map (map (++ ['.', '.']))) space) ++ [newW, newW]
  where (ax, ay, az, _) = add (2, 2, 2, 2) $ actualSize space
        newY = listN ax '.'
        newZ = listN ay newY
        newW = listN az newZ

listN n x = take n infX
  where infX = x:infX

activeNum :: Cube -> Int
activeNum '#' = 1
activeNum '.' = 0

countActive :: [Cube] -> Int
countActive = foldl (+) 0 . map activeNum

isActive :: Cube -> Bool
isActive '.' = False
isActive '#' = True

isInactive :: Cube -> Bool
isInactive = not . isActive

switchCube :: Cube -> Cube
switchCube '#' = '.'
switchCube '.' = '#'

cubeSurroundings :: Space -> Coords -> [Cube]
cubeSurroundings space pos = map (loc space . add pos) diffs
  where diffs = filter (/= origin) $ concat $ mapEach (concat $ mapEach (concat $ mapEach (map coords distances) distances) distances) distances
        distances = [-1, 0, 1]

evolveCube :: Space -> Coords -> Cube
evolveCube space pos | isActive cube && not (between 2 (countActive around) 3) = switchCube cube
                     | isInactive cube && countActive around == 3 = switchCube cube
                     | otherwise = cube
  where cube = loc space pos
        around = cubeSurroundings space pos

map4d :: (a -> b) -> [[[[a]]]] -> [[[[b]]]]
map4d f = map (map (map (map f)))

transformSpace :: (Space -> Coords -> b) -> Space -> [[[[b]]]]
transformSpace f space = map4d (f space . indexToCoord) $ map (\w -> map (\z -> map (\y -> map (\x -> coords x y z w) [0..sx]) [0..sy]) [0..sz]) [0..sw]
  where (sx, sy, sz, sw) = add (-1, -1, -1, -1) $ actualSize space

evolve :: Space -> Space
evolve = transformSpace evolveCube . surroundSpace

evolveN :: Int -> Space -> Space
evolveN 0 = id
evolveN n = evolveN (n-1) . evolve

createSpace :: [[Cube]] -> Space
createSpace plane = [[init $ concat $ map ((:[negativeSpace]) . init . concat . map (:['.'])) plane]]
  where negativeSpace = listN (2 * xsize - 1) '.'
        xsize = (length $ head plane)

flattenSpace = concat . concat . concat

mergeDoc = foldl ($$) (text "")

showSpace :: Space -> Doc
showSpace space = mergeDoc . map (mergeDoc . map (mergeDoc . (map (text . map (loc space))))) $ map (\w -> map (\z -> map (\y -> map (\x -> coords x y z w) [ax..bx]) [ay..by]) [az..bz]) [aw..bw]
  where ((ax, ay, az, aw), (bx, by, bz, bw)) = size space

main = do
  cnt <- getContents
  print $ countActive $ flattenSpace $ evolveN 6 $ createSpace $ lines cnt

