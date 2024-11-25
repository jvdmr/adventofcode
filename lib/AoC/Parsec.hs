{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC.Parsec
  ( module Text.Parsec
  , parens
  , brackets
  , braces
  , commaSep
  ) where

import Text.Parsec

parens :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
parens = between (char '(') (char ')')

brackets :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
brackets = between (char '[') (char ']')

commaSep :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
commaSep = flip sepBy (char ',')

braces :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
braces = between (char '{') (char '}')

