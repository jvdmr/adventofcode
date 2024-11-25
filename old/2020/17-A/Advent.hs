module Main where

-- import Data.List
import Text.PrettyPrint

between :: Int -> Int -> Int -> Bool
between a b c = a <= b && b <= c

mapEach :: [a -> b] -> [a] -> [[b]]
mapEach = flip $ map . flip map

type Cube = Char
type Space = [[[Cube]]]
type Coords = (Int, Int, Int)

coords x y z = (x, y, z)

add :: Coords -> Coords -> Coords
add (x1, y1, z1) (x2, y2, z2) = (x1+x2, y1+y2, z1+z2)

origin :: Coords
origin = coords 0 0 0

coordToIndex (x, y, z) = (c2i x, c2i y, c2i z)
  where c2i a | a < 0 = (a * (- 2)) - 1
              | otherwise = a * 2

indexToCoord (xi, yi, zi) = (i2c xi, i2c yi, i2c zi)
  where i2c a | even a = div a 2
              | otherwise = div (a + 1) (-2)

size :: Space -> (Coords, Coords)
size space = ((div x (-2), div y (-2), div z (-2)), (div x 2, div y 2, div z 2))
  where (x, y, z) = add (-1, -1, -1) (length $ head $ head space, length $ head space, length space)

actualSize :: Space -> (Int, Int, Int)
actualSize space = (length $ head $ head space, length $ head space, length space)

within :: Space -> Coords -> Bool
within space (x, y, z) = withinX && withinY && withinZ
  where ((ax, ay, az), (bx, by, bz)) = size space
        withinX = between ax x bx
        withinY = between ay y by
        withinZ = between az z bz

loc :: Space -> Coords -> Cube
loc space pos | within space pos = head $ drop xi $ head $ drop yi $ head $ drop zi space
              | otherwise = '.'
  where (xi, yi, zi) = coordToIndex pos

surroundSpace :: Space -> Space
surroundSpace space = (map (++ [newY, newY]) $ map (map (++ ['.', '.'])) space) ++ [newZ, newZ]
  where (ax, ay, _) = add (2, 2, 2) $ actualSize space
        newY = listN ax '.'
        newZ = listN ay newY

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
  where diffs = filter (/= origin) $ concat $ mapEach (concat $ mapEach (map coords distances) distances) distances
        distances = [-1, 0, 1]

evolveCube :: Space -> Coords -> Cube
evolveCube space pos | isActive cube && not (between 2 (countActive around) 3) = switchCube cube
                     | isInactive cube && countActive around == 3 = switchCube cube
                     | otherwise = cube
  where cube = loc space pos
        around = cubeSurroundings space pos

map3d :: (a -> b) -> [[[a]]] -> [[[b]]]
map3d f = map (map (map f))

transformSpace :: (Space -> Coords -> b) -> Space -> [[[b]]]
transformSpace f space = map3d (f space . indexToCoord) $ map (\z -> map (\y -> map (\x -> coords x y z) [0..sx]) [0..sy]) [0..sz]
  where (sx, sy, sz) = add (-1, -1, -1) $ actualSize space

evolve :: Space -> Space
evolve = transformSpace evolveCube . surroundSpace

evolveN :: Int -> Space -> Space
evolveN 0 = id
evolveN n = evolveN (n-1) . evolve

createSpace :: [[Cube]] -> Space
createSpace plane = [init $ concat $ map ((:[negativeSpace]) . init . concat . map (:['.'])) plane]
  where negativeSpace = listN (2 * xsize - 1) '.'
        xsize = (length $ head plane)

flattenSpace = concat . concat

mergeDoc = foldl ($$) (text "")

showSpace :: Space -> Doc
showSpace space = mergeDoc . map mergeDoc . map (map text) . map3d (loc space) $ map (\z -> map (\y -> map (\x -> coords x y z) [ax..bx]) [ay..by]) [az..bz]
  where ((ax, ay, az), (bx, by, bz)) = size space

-- debugging
p = [".#.","..#","###"]
s = createSpace p

main = do
  cnt <- getContents
  print $ showSpace $ createSpace $ lines cnt
  print $ showSpace $ evolveN 6 $ createSpace $ lines cnt
  print $ countActive $ flattenSpace $ evolveN 6 $ createSpace $ lines cnt

