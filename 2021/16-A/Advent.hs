{-# LANGUAGE BinaryLiterals #-}
module Main where

import Data.List
import Data.List.Split

import Text.Parsec
import Control.Monad.Identity (Identity)

import Debug.Trace
idtrace x = trace (show x) x
labelTrace s x = trace (s ++ show x) x

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
binToDec [] = 0
binToDec bs = 2 * binToDec (init bs) + read [last bs]

manyx :: Int -> ParsecT String u Identity a -> ParsecT String u Identity [a]
manyx 1 p =     (p >>= \r -> return [r])
            <?> "manyx"
manyx n p =     (p >>= \r -> manyx (n - 1) p >>= return . (r:))
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

bits :: Int -> ParsecT String u Identity String
bits n =     (manyx n bit >>= return)
         <?> ("bits " ++ show n)

numberHelp :: String -> ParsecT String u Identity Int
numberHelp prev =     (zero >> bits 4 >>= return . binToDec . (prev ++))
                  <|> (one >> bits 4 >>= numberHelp . (prev ++) >>= return)
                  <?> ("numberHelp " ++ prev)

number :: ParsecT String u Identity Int
-- number =     (numberHelp "" >>= return)
number =     (numberHelp "" >> return 0)
         <?> "number"

operation :: ParsecT String u Identity Int
operation =     (zero >> bits 15 >>= \l -> bits (binToDec l) >>= return . decypher)
            <|> (one >> bits 11 >>= \l -> manyx (binToDec l) packet >>= return . foldl (+) 0)
            <?> "operation"

decode :: Int -> ParsecT String u Identity Int
decode 4 =     (number >>= return)
           <?> "decode number 4"
decode n =     (operation >>= return)
           <?> ("decode operation " ++ show n)

typeid :: ParsecT String u Identity Int
typeid =     (bits 3 >>= return . binToDec)
         <?> "typeid"

version :: ParsecT String u Identity Int
version =     (bits 3 >>= return . binToDec)
          <?> "version"

packet :: ParsecT String u Identity Int
packet =     try (version >>= \v -> typeid >>= \t -> decode t >>= return . (v +))
         <|> (many1 (char '0') >> return 0)
         <?> "packet"

decypher = right . parse (many1 packet) "(source)"
  where right (Right ast) = foldl (+) 0 ast
        right (Left err) = error $ show err

main = do
  cnt <- getContents
  print $ map (decypher . idtrace . flatten . map hexToBin) $ lines cnt

