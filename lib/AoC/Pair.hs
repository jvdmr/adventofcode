{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC.Pair
  ( Pair (..)
  , add
  , combine
  , combine3
  , multiply
  , neg
  , pair
  , toList
  , unpair
  ) where

type Pair a = (a, a)

toList :: Pair a -> [a]
toList (a, b) = [a, b]

pair :: [a] -> Pair a
pair [a, b] = (a, b)

unpair :: (a -> b) -> Pair a -> Pair b
unpair f (a, b) = (f a, f b)

combine :: (a -> b -> c) -> Pair a -> Pair b -> Pair c
combine f (a, b) (c, d) = (f a c, f b d)

combine3 :: (a -> b -> c -> d) -> Pair a -> Pair b -> Pair c -> Pair d
combine3 f a = combine ($) . combine f a

add :: Num a => Pair a -> Pair a -> Pair a
add = combine (+)

neg :: Num a => Pair a -> Pair a
neg (a, b) = (-a, -b)

multiply :: Num a => a -> Pair a -> Pair a
multiply n = combine (*) (n, n)

