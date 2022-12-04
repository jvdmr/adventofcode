module Main where

import Data.List
import Data.List.Split

import Debug.Trace
idtrace x = trace (show x) x
gametrace boss player effects t = trace (show t ++ ": " ++ show boss ++ " - " ++ show player ++ " - " ++ show effects) t

data Spell = Spell {name::String, cost::Int, turns::Int, damage::Int, armor::Int, heal::Int, mana::Int}
  deriving (Eq, Show, Ord)

spells = [
  Spell "Magic Missile"  53 1 4 0 0   0,
  Spell "Drain"          73 1 2 0 2   0,
  Spell "Shield"        113 6 0 7 0   0,
  Spell "Poison"        173 6 3 0 0   0,
  Spell "Recharge"      229 5 0 0 0 101
  ]

data Player = Player {hp::Int, dmg::Int, mna::Int}
  deriving (Eq, Show, Ord)

player [hp, d, m] = Player hp d m

value :: String -> Int
value (':':' ':num) = read num
value (_:rst) = value rst

data Turn = BTurn | PTurn
  deriving (Eq, Show, Ord)

data Effect = Effect {spell::Spell, turnsLeft::Int}
  deriving (Eq, Show, Ord)

cast :: Spell -> Effect
cast s = Effect s $ turns s

nextTurn :: [Effect] -> [Effect]
nextTurn es = filter wornOff $ map nt es
  where nt e = e {turnsLeft = (turnsLeft e) - 1}
        wornOff e = 0 < turnsLeft e

totaldmg es = foldl (+) 0 $ map (damage . spell) es
totalarmor es = foldl (+) 0 $ map (armor . spell) es
totalheal es = foldl (+) 0 $ map (heal . spell) es
totalmana es = foldl (+) 0 $ map (mana . spell) es

uniq [] = []
uniq [a] = [a]
uniq (a:b:rst) | a == b = uniq (b:rst)
               | otherwise = a:uniq (b:rst)

type GameState = (Int, Turn, Player, Player, [Effect])

fight :: [GameState] -> [Int]
fight [] = []
fight ((total, _, (Player 0 _ _), (Player _ _ _), _):rest) = total:fight rest
fight ((_, _, (Player _ _ _), (Player 0 _ _), _):rest) = fight rest
fight ((total, BTurn, boss, player, effects):rest) = fight $ uniq $ sort $ nextFight:rest
  where nextFight = (total, gametrace boss player effects PTurn, boss { hp = max 0 $ (hp boss) - (totaldmg effects) }, player { hp = max 0 $ (hp player) - ((dmg boss) - (totalarmor effects)) + (totalheal effects), mna = max 0 $ (mna player) + (totalmana effects) }, sort $ nextTurn effects)
fight ((total, PTurn, boss, player, effects):rest) | mna player < 53 = fight rest
                                                   | otherwise = fight $ uniq $ sort nextFights
  where nextFights = (map nextFight $ filter castable spells) ++ rest
        nextFight s = (total + cost s, BTurn, boss { hp = max 0 $ (hp boss) - (totaldmg effects) }, player { hp = max 0 $ (hp player) + (totalheal effects), mna = max 0 $ (mna player) - (cost s) + (totalmana effects) }, sort $ cast s:nextEffects)
        castable s = notElem s (map spell nextEffects) && cost s <= mna player
        nextEffects = nextTurn effects

findCheapest (player:boss:[]) = head $ fight [(0, PTurn, boss, player, [])]

main = do
  cnt <- getContents
  print $ findCheapest $ map player $ chunksOf 3 $ map value $ lines cnt

