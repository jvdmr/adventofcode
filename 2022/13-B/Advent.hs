module Main where

import Data.List
import Data.List.Split hiding (sepBy)

import Text.Parsec

import Debug.Trace
idtrace x = trace (show x) x

data Packet = PacketLst [Packet]
            | PacketNum Int
            deriving (Eq, Show)

parens :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
parens = between (string "(") (string ")")

brackets :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
brackets = between (string "[") (string "]")

commaSep :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
commaSep = flip sepBy (string ",")

packet =     (brackets (commaSep packet) >>= return . PacketLst)
         <|> (many1 digit >>= return . PacketNum . read)
         <?> "packet"

parsePacket :: String -> Packet
parsePacket = right . parse packet "(source)"
  where right (Right ast) = ast

cons [a, b] = (a, b)

comparePackets ((a, b):n) (PacketLst []) (PacketLst []) = comparePackets n a b
comparePackets _ (PacketLst []) (PacketLst _) = LT
comparePackets _ (PacketLst _) (PacketLst []) = GT
comparePackets n (PacketLst ((PacketLst a):as)) (PacketLst ((PacketLst b):bs)) = comparePackets ((PacketLst as, PacketLst bs):n) (PacketLst a) (PacketLst b)
comparePackets n (PacketLst ((PacketNum a):as)) (PacketLst ((PacketLst b):bs)) = comparePackets n (PacketLst ((PacketLst [PacketNum a]):as)) (PacketLst ((PacketLst b):bs))
comparePackets n (PacketLst ((PacketLst a):as)) (PacketLst ((PacketNum b):bs)) = comparePackets n (PacketLst ((PacketLst a):as)) (PacketLst ((PacketLst [PacketNum b]):bs))
comparePackets n (PacketLst ((PacketNum a):as)) (PacketLst ((PacketNum b):bs)) | a < b = LT
                                                                               | a > b = GT
                                                                               | otherwise = comparePackets n (PacketLst as) (PacketLst bs)

dividers = map parsePacket ["[[2]]", "[[6]]"]

main = do
  cnt <- getContents
  print $ foldl (*) 1 $ map fst $ map idtrace $ filter (flip elem dividers . snd) $ zip [1..] $ sortBy (comparePackets []) $ (++ dividers) $ map parsePacket $ filter (/= "") $ lines cnt

