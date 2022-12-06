module Main where

import Data.List

import Text.Parsec

import Debug.Trace
idtrace x = trace (show x) x

marker =     char '(' >> many1 digit >>= \n -> char 'x' >> many1 digit >>= \l -> char ')' >> count (read n) anyChar >>= return . concat . take (read l) . repeat
         <?> "marker"

nomarker =     many1 letter
           <?> "nomarker"

compressedPart =     try marker
                 <|> nomarker
                 <?> "comporessedPart"

compressed =     many1 compressedPart >>= return . concat
             <?> "compressed"

decompress :: String -> String
decompress = right . parse compressed "(source)"
  where right (Right ast) = ast

main = do
  cnt <- getContents
  print $ map length $ map decompress $ lines cnt

