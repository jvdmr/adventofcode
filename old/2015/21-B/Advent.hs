module Main where

-- import Data.List
import Data.List.Split

import Debug.Trace
idtrace x = trace (show x) x

data ItemKind = Weapon | Armor | Ring
  deriving (Eq, Show)

data Item = Item ItemKind String Int Int Int
          | None
  deriving (Eq, Show)

kind   (Item k _ _ _ _) = k
name   (Item _ n _ _ _) = n
cost   (Item _ _ c _ _) = c
cost   None = 0
damage (Item _ _ _ d _) = d
damage None = 0
armor  (Item _ _ _ _ a) = a
armor  None = 0

weapon (n, c, d, a) = Item Weapon n c d a
shield (n, c, d, a) = Item Armor  n c d a
ring   (n, c, d, a) = Item Ring   n c d a

weapons = map weapon [
  ("Dagger",      8, 4, 0),
  ("Shortsword", 10, 5, 0),
  ("Warhammer",  25, 6, 0),
  ("Longsword",  40, 7, 0),
  ("Greataxe",   74, 8, 0)
  ]

armors = None:map shield [
  ("Leather",     13, 0, 1),
  ("Chainmail",   31, 0, 2),
  ("Splintmail",  53, 0, 3),
  ("Bandedmail",  75, 0, 4),
  ("Platemail",  102, 0, 5)
  ]

rings = None:None:map ring [
  ("Damage +1",   25, 1, 0),
  ("Damage +2",   50, 2, 0),
  ("Damage +3",  100, 3, 0),
  ("Defense +1",  20, 0, 1),
  ("Defense +2",  40, 0, 2),
  ("Defense +3",  80, 0, 3)
  ]

sumItems itemList = foldl (+) 0 $ map cost itemList

data Player = Player [Item] Int Int Int
  deriving (Eq, Show)

items (Player itemList _ _ _) = itemList

setItems (Player _ hp d a) itemList = Player itemList hp d a

instance Ord Player where
  compare a b = compare cb ca
    where ca = sumItems $ items a
          cb = sumItems $ items b

value :: String -> Int
value (':':' ':num) = read num
value (_:rst) = value rst

player [hp, d, a] = Player [] hp d a

equip p = map (setItems p) [[w, a, r1, r2] | w <- weapons, a <- armors, r1 <- rings, r2 <- rings, r1 == None && r2 == None || r1 /= r2]

fight (Player _ 0 _ _) (Player _ _ _ _) = False
fight (Player _ _ _ _) (Player _ 0 _ _) = True
fight (Player [] bosshp bossd bossa) (Player [] playerhp playerd playera) = fight (Player [] (max 0 $ bosshp - (playerd - bossa)) bossd bossa) (Player [] (max 0 $ playerhp - (bossd - playera)) playerd playera)
fight (Player [] bosshp bossd bossa) (Player itemList hp d a) = fight (Player [] bosshp bossd bossa) (Player [] playerhp playerd playera)
  where playerhp = hp
        playerd = foldl (+) d $ map damage itemList
        playera = foldl (+) a $ map armor itemList

findCheapest (player:boss:[]) = head $ sort $ filter (fight boss) $ equip player

main = do
  cnt <- getContents
  print $ sumItems $ items $ idtrace $ findCheapest $ map player $ chunksOf 3 $ map value $ lines cnt

