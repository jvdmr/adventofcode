module Main where

-- import Data.List

import Text.Parsec

import Debug.Trace
idtrace x = trace (show x) x

marker =     char '(' >> many1 digit >>= \n -> char 'x' >> many1 digit >>= \l -> char ')' >> count (read n) anyChar >>= \s -> return (read l, s)
         <?> "marker"

nomarker =     many1 letter >>= return . length
           <?> "nomarker"

compressedPart =     try (marker >>= \(l, s) -> return $ l * decompress s)
                 <|> nomarker
                 <?> "comporessedPart"

compressed =     many1 compressedPart >>= return . sum
             <?> "compressed"

decompress :: String -> Int
decompress = right . parse compressed "(source)"
  where right (Right ast) = ast

main = do
  cnt <- getContents
  print $ map decompress $ lines cnt

