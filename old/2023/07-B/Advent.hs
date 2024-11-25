module Main where

-- import Data.List
import Data.List.Split (splitOn)

import Debug.Trace
idtrace x = trace (show x) x

data Card = J | P Int | T | Q | K | A
  deriving (Show, Eq, Ord)

readCard :: Char -> Card
readCard 'T' = T
readCard 'J' = J
readCard 'Q' = Q
readCard 'K' = K
readCard 'A' = A
readCard n = P $ read [n]

type Hand = [Card]

compareCards :: Hand -> Hand -> Ordering
compareCards [] _ = EQ
compareCards (a:as) (b:bs) | a == b = compareCards as bs
                           | otherwise = compare a b

data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind
  deriving (Show, Eq, Ord)

hasJoker :: Hand -> Bool
hasJoker = any ((==) J)

isFiveOfAKind' :: Hand -> Bool
isFiveOfAKind' h = (isFiveOfAKind h) || ((isFourOfAKind h || isFullHouse h) && hasJoker h)

isFiveOfAKind :: Hand -> Bool
isFiveOfAKind = (==) 1 . length . nub

isFourOfAKind' :: Hand -> Bool
isFourOfAKind' h = (isFourOfAKind h)
               || (isThreeOfAKind h && hasJoker h)
               || (isTwoPair h && (any ((==) [J, J]) $ group $ sort h))

isFourOfAKind :: Hand -> Bool
isFourOfAKind = any ((==) 4 . length) . group . sort

isFullHouse' :: Hand -> Bool
isFullHouse' h = (isFullHouse h) || (isTwoPair h && hasJoker h)

isFullHouse :: Hand -> Bool
isFullHouse = (==) 2 . length . nub

isThreeOfAKind' :: Hand -> Bool
isThreeOfAKind' h = (isThreeOfAKind h) || (isOnePair h && hasJoker h)

isThreeOfAKind :: Hand -> Bool
isThreeOfAKind = any ((==) 3 . length) . group . sort

isTwoPair :: Hand -> Bool
isTwoPair = (==) 3 . length . nub

isOnePair' :: Hand -> Bool
isOnePair' h = isOnePair h || hasJoker h

isOnePair :: Hand -> Bool
isOnePair = (==) 4 . length . nub

handType :: Hand -> HandType
handType hand | isFiveOfAKind' hand = FiveOfAKind
              | isFourOfAKind' hand = FourOfAKind
              | isFullHouse' hand = FullHouse
              | isThreeOfAKind' hand = ThreeOfAKind
              | isTwoPair hand = TwoPair
              | isOnePair' hand = OnePair
              | otherwise = HighCard

type Bid = Int
type HandData = (Hand, HandType, Bid)

bid :: HandData -> Bid
bid (_, _, b) = b

compareHands :: HandData -> HandData -> Ordering
compareHands (handA, handTypeA, _) (handB, handTypeB, _) | handTypeA == handTypeB = compareCards handA handB
                                                         | otherwise = compare handTypeA handTypeB

readHandData :: String -> HandData
readHandData s = (hand, handType hand, read bid)
  where [cards, bid] = splitOn " " s
        hand = map readCard cards

winnings :: [HandData] -> Int
winnings = sum . zipWith (*) [1..] . map bid . sortBy compareHands

main = do
  cnt <- getContents
  print $ winnings $ map readHandData $ lines cnt

