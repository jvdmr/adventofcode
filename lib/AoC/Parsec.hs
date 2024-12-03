{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC.Parsec
  ( module Text.Parsec
  , brackets
  , braces
  , commaSep
  , integer
  , parens
  , right
  ) where

import Text.Parsec

right :: Show a => Either a b -> b
right (Right ast) = ast
right (Left x) = error $ show x

parens :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
parens = between (char '(') (char ')')

brackets :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
brackets = between (char '[') (char ']')

commaSep :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
commaSep = flip sepBy (char ',')

braces :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
braces = between (char '{') (char '}')

integer :: Stream s m Char => ParsecT s u m Int
integer = many1 digit >>= return . read

