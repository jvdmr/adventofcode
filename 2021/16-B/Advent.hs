module Main where

import Data.List
import Data.List.Split

import Text.Parsec
import Control.Monad.Identity (Identity)

import Debug.Trace
idtrace x = trace (show x) x
labelTrace t s x = trace (t ++ s ++ ": " ++ show x) x

flatten :: [[a]] -> [a]
flatten = foldl (++) []

hexToBin :: Char -> String
hexToBin '0' = "0000"
hexToBin '1' = "0001"
hexToBin '2' = "0010"
hexToBin '3' = "0011"
hexToBin '4' = "0100"
hexToBin '5' = "0101"
hexToBin '6' = "0110"
hexToBin '7' = "0111"
hexToBin '8' = "1000"
hexToBin '9' = "1001"
hexToBin 'A' = "1010"
hexToBin 'B' = "1011"
hexToBin 'C' = "1100"
hexToBin 'D' = "1101"
hexToBin 'E' = "1110"
hexToBin 'F' = "1111"

binToDec :: String -> Int
binToDec = foldl (+) 0 . zipWith (*) twos . reverse . map (read . (:[]))
  where twos = 1:[2 * x | x <- twos]

bmanyx :: Int -> ParsecT String u Identity a -> ParsecT String u Identity [a]
bmanyx 1 p =     (p >>= \r -> return [r])
            <?> "bmanyx"
bmanyx n p =     (p >>= \r -> bmanyx (n - 1) p >>= return . (r:))
            <?> ("bmanyx " ++ show n)

manyx :: Show a => String -> Int -> (String -> ParsecT String u Identity a) -> ParsecT String u Identity [a]
manyx tag 1 p =     (parserTrace (tag ++ "manyx 1") >> p (tag ++ "m1| ") >>= \r -> return (labelTrace tag ">> manyx 1" [r]))
            <?> "manyx"
manyx tag n p =     (parserTrace (tag ++ "manyx " ++ show n) >> p (tag ++ "m" ++ show n ++ "| ") >>= \r -> manyx tag (n - 1) p >>= return . labelTrace tag (">> manyx " ++ show n) . (r:))
            <?> ("manyx " ++ show n)

zero :: ParsecT String u Identity Char
zero =     (char '0' >>= return)
       <?> "zero"

one :: ParsecT String u Identity Char
one =     (char '1' >>= return)
      <?> "one"

bit :: ParsecT String u Identity Char
bit =     (zero >>= return)
      <|> (one >>= return)
      <?> "bit"

bits :: String -> Int -> ParsecT String u Identity String
bits tag n =     (parserTrace (tag ++ "bits " ++ show n) >> bmanyx n bit >>= return)
         <?> ("bits " ++ show n)

numberHelp :: String -> String -> ParsecT String u Identity Int
numberHelp tag prev =     (zero >> parserTrace (tag ++ "numberHelp " ++ prev) >> bits tag 4 >>= return . labelTrace tag ">> numberHelp binToDec" . binToDec . labelTrace tag ">> numberHelp read" . (prev ++))
                  <|> (one >> parserTrace (tag ++ "numberHelp " ++ prev) >> bits tag 4 >>= numberHelp tag . (prev ++) >>= return)
                  <?> ("numberHelp " ++ prev)

number :: String -> ParsecT String u Identity Int
number tag =     (parserTrace (tag ++ "number") >> numberHelp tag "" >>= return . labelTrace tag ">> number")
         <?> "number"

packetlist :: String -> ParsecT String u Identity [Int]
packetlist tag =     (zero >> parserTrace (tag ++ "packetlist 15") >> bits tag 15 >>= \l -> bits tag (binToDec l) >>= return . labelTrace tag ">> packetlist 15" . decypher (tag ++ "| "))
             <|> (one >> parserTrace (tag ++ "packetlist 11") >> bits tag 11 >>= \l -> manyx tag (binToDec l) packet >>= return . labelTrace tag ">> packetlist 11" . filter (>= 0))
             <?> "packetlist"

comp :: Ord a => (a -> a -> Bool) -> [a] -> Int
comp f (a:b:_) | f a b = 1
               | otherwise = 0

operation :: String -> Int -> ([Int] -> Int)
operation tag 0 = labelTrace tag ">> (+)" . foldl (+) 0 . labelTrace tag "(+)" -- sum
operation tag 1 = labelTrace tag ">> (*)" . foldl (*) 1 . labelTrace tag "(*)" -- product
operation tag 2 = labelTrace tag ">> (min)" . head . sort . labelTrace tag "(min)" -- minimum
operation tag 3 = labelTrace tag ">> (max)" . last . sort . labelTrace tag "(max)" -- maximum
operation tag 5 = labelTrace tag ">> (>)" . comp (>) . labelTrace tag "(>)" -- greater than
operation tag 6 = labelTrace tag ">> (<)" . comp (<) . labelTrace tag "(<)" -- less than
operation tag 7 = labelTrace tag ">> (==)" . comp (==) . labelTrace tag "(==)" -- equal to

decode :: String -> Int -> ParsecT String u Identity Int
decode tag 4 =     (parserTrace (tag ++ "decode number 4") >> number tag >>= return . labelTrace tag ">> decode number 4")
           <?> "decode number 4"
decode tag n =     (parserTrace (tag ++ "decode operation " ++ show n) >> packetlist tag >>= return . labelTrace tag (">> decode operation " ++ show n) . operation tag n)
           <?> ("decode packetlist " ++ show n)

typeid :: String -> ParsecT String u Identity Int
typeid tag =     (parserTrace (tag ++ "typeid") >> bits tag 3 >>= return . labelTrace tag ">> typeid" . binToDec)
         <?> "typeid"

version :: String -> ParsecT String u Identity Int
version tag =     (parserTrace (tag ++ "version") >> bits tag 3 >>= return . labelTrace tag ">> version" . binToDec)
          <?> "version"

packet :: String -> ParsecT String u Identity Int
packet tag =     try (parserTrace (tag ++ "packet") >> version tag >> typeid tag >>= \t -> decode tag t >>= return . labelTrace tag ">> packet")
         <|> (parserTrace (tag ++ "rest") >> many1 (char '0') >> return (labelTrace tag ">> rest" (-1)))
         <?> "packet"

decypher :: String -> String -> [Int]
decypher tag = right . parse (many1 (packet tag)) "(source)" . labelTrace tag "decypher"
  where right (Right ast) = labelTrace tag ">> decypher" $ filter (>= 0) ast
        right (Left err) = error $ show err

main = do
  cnt <- getContents
  print $ map (head . decypher "" . flatten . map hexToBin) $ lines cnt

