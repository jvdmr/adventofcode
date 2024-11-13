module Main where

import Data.Ord
import Data.List hiding (empty, insert)
import Data.Map (insert, findWithDefault)
import qualified Data.Map as M

import Debug.Trace
idtrace x = trace (show x) x
ftrace f x = trace (f x) x

-- uniq is better than nub on sorted lists
uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq [a] = [a]
uniq (a:b:rst) | a == b = uniq (b:rst)
               | otherwise = a:uniq (b:rst)

data Color = Amber | Bronze | Copper | Desert deriving (Eq, Enum, Ord)

instance Show Color where
  show Amber = "A"
  show Bronze = "B"
  show Copper = "C"
  show Desert = "D"

data Amphipod = Amphipod {color::Color, energy::Int}
              | Empty

instance Show Amphipod where
  show Empty = "."
  show a = show $ color a

instance Eq Amphipod where
  Empty == Empty = True
  Amphipod a _ == Amphipod b _ = a == b
  _ == _ = False

instance Ord Amphipod where
  compare Empty Empty = EQ
  compare Empty _ = LT
  compare _ Empty = GT
  compare (Amphipod a _) (Amphipod b _) = compare a b

empty = (== Empty)

matchColor :: Char -> Color
matchColor 'A' = Amber
matchColor 'B' = Bronze
matchColor 'C' = Copper
matchColor 'D' = Desert

amphipod :: Char -> Amphipod
amphipod c = Amphipod (matchColor c) 0

usageColor :: Color -> Int
usageColor Amber = 1
usageColor Bronze = 10
usageColor Copper = 100
usageColor Desert = 1000

usage :: Amphipod -> Int
usage (Amphipod c _) = usageColor c

addEnergy :: Amphipod -> Int -> Amphipod
addEnergy (Amphipod c e) addE = Amphipod c (e + addE)

data BurrowSpace = Space {occupant::Amphipod}
                 | Door {purpose::Color, room::[Amphipod]}
                 deriving (Eq, Ord)

instance Show BurrowSpace where
  show (Space a) = show a
  show (Door _ as) = concat $ ".":map show as

data Burrow = Burrow [BurrowSpace]
            | Blank
            deriving (Eq, Ord)

instance Show Burrow where
  show (Burrow burrow) = concat $ map (++ "\n") $ take 3 $ transpose $ map ((++ "####") . show) burrow

makeDoors :: BurrowSpace -> BurrowSpace -> BurrowSpace
makeDoors (Space _) (Space _) = Space Empty
makeDoors door@(Door _ _) (Space _) = door

makeRooms' :: [Color] -> [[Char]] -> [BurrowSpace]
makeRooms' _ [] = []
makeRooms' purposes (('#':_):rest) = Space Empty:makeRooms' purposes rest
makeRooms' purposes ((' ':_):rest) = Space Empty:makeRooms' purposes rest
makeRooms' (p:purposes) (as:rest) = Door p (map amphipod as):makeRooms' purposes rest

makeRooms :: [[Char]] -> [BurrowSpace]
makeRooms = makeRooms' [Amber, Bronze, Copper, Desert]

parseBurrow :: [String] -> Burrow
parseBurrow (_:s:as) = Burrow $ zipWith makeDoors rooms $ map (\'.' -> Space Empty) $ init $ tail s
  where rooms = makeRooms $ tail $ occupants $ map (++ "###") $ take 2 as
        occupants ([]:_) = []
        occupants ls = (map head ls):occupants (map tail ls)

clearSpace :: BurrowSpace -> Bool
clearSpace (Space Empty) = True
clearSpace (Door p room) = True
clearSpace _ = False

clearDestination :: Color -> BurrowSpace -> Bool
clearDestination _ (Space Empty) = True
clearDestination c (Door c' as) = c == c' && (foldl (&&) True $ map ((== c) . color) $ dropWhile empty as)
clearDestination _ _ = False

organizedSpace :: BurrowSpace -> Bool
organizedSpace (Space Empty) = True
organizedSpace (Door p room) = room == (filter ((== p) . color) $ dropWhile empty room)
organizedSpace _ = False

organizedBurrow :: Burrow -> Bool
organizedBurrow (Burrow burrow) = foldl (&&) True $ map organizedSpace burrow

countOrganizedRoom :: BurrowSpace -> Int
countOrganizedRoom (Space Empty) = 0
countOrganizedRoom (Door p room) | last room == Empty = 0
                                 | otherwise = length $ takeWhile ((== p) . color) $ takeWhile (not . empty) $ reverse room
countOrganizedRoom _ = 0

getAmphipod :: BurrowSpace -> Amphipod
getAmphipod (Space a) = a
getAmphipod (Door _ as) = head $ dropWhile empty as

amphipodPosition :: BurrowSpace -> Int
amphipodPosition (Space a) = 0
amphipodPosition (Door _ as) = 1 + length (takeWhile empty as)

removeAmphipod :: BurrowSpace -> BurrowSpace
removeAmphipod (Space _) = Space Empty
removeAmphipod (Door c as) = Door c (takeWhile empty as ++ [Empty] ++ tail (dropWhile empty as))

putAmphipod :: BurrowSpace -> Amphipod -> BurrowSpace
putAmphipod (Space Empty) a = Space a
putAmphipod (Door c as) a = Door c (init (takeWhile empty as) ++ [a] ++ (dropWhile empty as)) 

moveAmphipod :: Burrow -> Int -> Int -> (Int, Burrow)
moveAmphipod (Burrow burrow) i j = (e, Burrow $ zipWith newRoom [0..] burrow)
  where e = (abs (i - j) + pos) * usage a
        start = burrow !! i
        a = getAmphipod start
        pos = amphipodPosition start + amphipodPosition to
        from = removeAmphipod start
        to = putAmphipod (burrow !! j) $ addEnergy a e
        newRoom i' r | i' == i = from
                     | i' == j = to
                     | otherwise = r

clearPath :: Burrow -> Int -> Int -> Bool
clearPath (Burrow burrow) i j | i == j = False
                              | space (burrow !! i) && space (burrow !! j) = False
                              | otherwise = clearDestination (color $ getAmphipod $ burrow !! i) (burrow !! j) && foldl (&&) True (map clearSpace path)
  where path | i < j = map (burrow !!) [i + 1..j]
             | otherwise = map (burrow !!) [j..i - 1]

reachableFrom :: Burrow -> Int -> [Int]
reachableFrom burrow@(Burrow bs) i = destinations
  where destinations = filter (clearPath burrow i) $ take (length bs) [0..]

possibleMovesFrom :: Burrow -> Int -> [(Int, Burrow)]
possibleMovesFrom burrow@(Burrow bs) i = case bs !! i of
                                              Space Empty -> []
                                              Space _ -> possibilities
                                              d@(Door c as) | organizedSpace d -> []
                                                            | noEmpties as == [] -> []
                                                            | foldl (&&) True $ map ((== c) . color) $ noEmpties as -> []
                                                            | otherwise -> possibilities
                                              where possibilities = map (moveAmphipod burrow i) $ reachableFrom burrow i
                                                    noEmpties as = dropWhile empty as

possibleMoves :: Burrow -> [(Int, Burrow)]
possibleMoves burrow@(Burrow bs) | organizedBurrow burrow = []
                                 | otherwise = concat $ map (possibleMovesFrom burrow) $ take (length bs) [0..]

scoreSpace :: BurrowSpace -> Int
scoreSpace (Space Empty) = 0
scoreSpace (Space a) = energy a
scoreSpace (Door _ as) = foldl (+) 0 $ map energy $ dropWhile empty as

score :: Burrow -> Int
score Blank = maxBound
score (Burrow burrow) = foldl (+) 0 $ map scoreSpace burrow

fixed :: Burrow -> Int
fixed (Burrow b) = sum $ map countOrganizedRoom b

fixedAndScore ((na, a):_) ((nb, b):_) | fa == fb = compare nb na
                                      | na == nb = comparing score a b
                                      | otherwise = compare fb fa
                                      where fa = fixed a
                                            fb = fixed b

door :: BurrowSpace -> Bool
door (Space _) = False
door (Door _ _) = True

space :: BurrowSpace -> Bool
space = not . door

pretty :: [Burrow] -> String
pretty = foldl (\a b -> a ++ "\n\n" ++ (show $ score b) ++ "\n" ++ show b) "" . reverse

notBlank [(_, Blank)] = False
notBlank _ = True

dfs' _ result [] = score $ head $ ftrace pretty $ map snd result
dfs' seen result (history@((_, q):_):queue) | notBlank result && score q > score (snd $ head result) = dfs' seen result queue
                                            | otherwise = dfs' (foldr (\(_, p) -> insert p (score p)) seen possibilities) newResult $ unfinished ++ (filter (flip notElem (map (snd . head) unfinished) . snd . head) queue)
                           where possibilities = filter seenSmallerScore $ possibleMoves q
                                 newResult = head $ sortBy (comparing $ score . snd . head) $ result:(map (ftrace (pretty . map snd) . (:history)) $ filter (organizedBurrow . snd) possibilities)
                                 unfinished = map (:history) $ sort $ filter ((< score (snd $ head newResult)) . score . snd) $ filter (not . organizedBurrow . snd) possibilities
                                 seenSmallerScore (_, p) = score p < findWithDefault maxBound p seen

dfs burrow = dfs' (insert burrow 0 M.empty) [(0, Blank)] [[(0, burrow)]]

main = do
  cnt <- getContents
  print $ dfs $ parseBurrow $ lines cnt

