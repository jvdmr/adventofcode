module Main where

import Prelude hiding (showList)
-- import Data.List
import Data.Char (toUpper)
import Data.List.Split

import Debug.Trace
idtrace x = trace (show x) x
ftrace f x = trace (f x) x

data Item = Chip String
          | Generator String
          | Elevator Int
--           deriving (Show)

instance Show Item where
  show (Chip s) = [' ', toUpper $ head s, 'M']
  show (Generator s) = [' ', toUpper $ head s, 'G']
  show (Elevator n) = "E" ++ show n

instance Eq Item where
  (Chip a) == (Chip b) = a == b
  (Generator a) == (Generator b) = a == b
  (Elevator _) == (Elevator _) = True
  _ == _ = False

instance Ord Item where
  compare (Chip a) (Chip b) = compare a b
  compare (Generator a) (Generator b) = compare a b
  compare (Chip _) (Generator _) = LT
  compare (Generator _) (Chip _) = GT
  compare (Elevator _) (Elevator _) = EQ
  compare (Elevator _) _ = LT
  compare _ (Elevator _) = GT

type Floor = [Item]
type Building = [Floor]
type History = [Building]

showList :: Show a => [a] -> String
showList = concat . map ((++ "\n") . show)

showHistory :: History -> String
showHistory = concat . map ((++ "\n") . showList . reverse) . reverse

parseItem :: [String] -> Item
parseItem [t] = Generator t
parseItem [t, "compatible"] = Chip t

parseFloor :: [String] -> Floor
parseFloor f = sort $ map parseItem $ map (splitOn "-" . head) $ drop 1 $ splitOn ["a"] f

addElevator :: Building -> Building
addElevator (firstFloor:floors) = ((Elevator 0):firstFloor):floors

isEmpty [] = True
isEmpty _ = False

isElevator :: [Item] -> Bool
isElevator ((Elevator _):_) = True
isElevator _ = False

isChip :: Item -> Bool
isChip (Chip _) = True
isChip _ = False

isGenerator :: Item -> Bool
isGenerator (Generator _) = True
isGenerator _ = False

hasChip :: [Item] -> String -> Bool
hasChip is s = elem (Chip s) is

hasGenerator :: [Item] -> String -> Bool
hasGenerator is s = elem (Generator s) is

inc :: Item -> Item
inc (Elevator n) = Elevator (n + 1)

makeEmpty a b | a == b = []
              | otherwise = [b]

kind :: Item -> String
kind (Chip n) = n
kind (Generator n) = n

illegalFloor :: Floor -> Bool
illegalFloor floor = 0 < length unmatchedChips && 0 < length generators
  where generators = filter isGenerator floor
        unmatchedChips = filter (not . hasGenerator floor . kind) $ filter isChip floor

uniq [] = []
uniq [a] = [a]
uniq (a:b:rst) | a == b = uniq (b:rst)
               | otherwise = a:uniq (b:rst)

move :: History -> [History]
move history@(floors:_) = map (:history) $ filter (not . illegal) $ uniq $ sort $ concat $ map (moveTo any2) tofloors
  where nfloors = zip [1..] floors
        efloor = head $ filter (isElevator . snd) nfloors
        fn = fst efloor
        elevator = inc $ head $ snd efloor
        fromfloor = tail $ snd efloor
        tofloors = filter ((== 1) . abs . (fn -) . fst) nfloors
        otherfloors tofloor = filter (flip notElem [efloor, tofloor]) nfloors
        newfromfloor is = (fn, sort $ filter (flip notElem is) fromfloor)
        moveTo isl tofloor = map (moveToFloor tofloor) isl
        moveToFloor tofloor@(n, tis) is = map snd $ sort $ (newfromfloor is):(n, sort $ elevator:tis ++ is):otherfloors tofloor
        any2 = filter (not . illegalFloor) $ concat $ map (\i -> map ((i:) . makeEmpty i) fromfloor) fromfloor

illegal :: Building -> Bool
illegal = any illegalFloor

value :: Item -> Int
value (Elevator n) = n

bringToTop :: Either ([Building], [History]) Int -> Either ([Building], [History]) Int
bringToTop (Left (tried, (history@(floors:_):rest))) | all isEmpty (init floors) = Right $ value $ head $ last $ trace (showHistory history) floors
--                                                      | illegal floors = Left (tried, rest)
                                                     | otherwise = Left ((tried ++ map head nextMoves), (rest ++ nextMoves))
                                                     where nextMoves = filter (flip notElem tried . head) $ move $ ftrace (showList . reverse . head) history
--                                                      where nextMoves = filter (flip notElem tried . head) $ move history

format :: Building -> Either ([Building], [History]) Int
format b = Left ([b], [[b]])

fromRight (Right r) = r
fromLeft (Left l) = l

bringToTop' :: Building -> Int
bringToTop' b = fromRight $ head $ dropWhile isLeft $ iterate bringToTop $ format b
  where isLeft (Left _) = True
        isLeft _ = False

main = do
  cnt <- getContents
--   print $ ftrace (concat . map ((++ "\n") . showHistory) . snd) $ fromLeft $ bringToTop $ format $ addElevator $ map (parseFloor . words) $ lines cnt
--   print $ ftrace (showHistory . fst) $ fromLeft $ bringToTop $ format $ addElevator $ map (parseFloor . words) $ lines cnt
  print $ bringToTop' $ addElevator $ map (parseFloor . words) $ lines cnt

