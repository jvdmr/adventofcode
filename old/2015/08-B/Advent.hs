module Main where

import Data.Char (chr)
-- import Data.List
import Text.Parsec

data Expr = AChar Char
          | Backslash
          | DoubleQuote
          | EscapedDoubleQuote
          | Hex Int
          deriving (Eq)

instance Show Expr where
  show (AChar c) = show c
  show (Backslash) = "\\\\"
  show (DoubleQuote) = "\""
  show (EscapedDoubleQuote) = "\\\""
  show (Hex i) = show $ chr i

expr =     try (string "\\\\" >> return Backslash)
       <|> try (string "\\\"" >> return EscapedDoubleQuote)
       <|> try (string "\\x" >> count 2 hexDigit >>= return . Hex . read)
       <|> (alphaNum >>= return . AChar)
       <?> "expr"

doublequoted =     char '"' >> many1 expr >>= \a -> (char '"' >> return ((DoubleQuote:a) ++ [DoubleQuote]))
               <?> "doublequoted"

parseString :: String -> [Expr]
parseString = right . parse doublequoted "(source)"
  where right (Right ast) = ast
        right (Left a) = error $ show a

countStrCode [] = 0
countStrCode ((AChar _):rst) = 1 + countStrCode rst
countStrCode (Backslash:rst) = 2 + countStrCode rst
countStrCode (DoubleQuote:rst) = 1 + countStrCode rst
countStrCode (EscapedDoubleQuote:rst) = 2 + countStrCode rst
countStrCode ((Hex _):rst) = 4 + countStrCode rst

countStrValues [] = 0
countStrValues ((AChar _):rst) = 1 + countStrValues rst
countStrValues (Backslash:rst) = 1 + countStrValues rst
countStrValues (DoubleQuote:rst) = countStrValues rst
countStrValues (EscapedDoubleQuote:rst) = 1 + countStrValues rst
countStrValues ((Hex _):rst) = 1 + countStrValues rst

applyBoth fa fb x = (fa x, fb x)

main = do
  cnt <- getContents
  print $ foldl (+) 0 $ map (uncurry (-) . applyBoth countStrCode countStrValues . parseString . show) $ lines cnt

