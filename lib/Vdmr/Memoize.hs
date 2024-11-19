{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module Vdmr.Memoize
  ( MemoizeableFunction (..)
  , memoize
  ) where

type MemoizeableFunction a b = ((a -> b) -> a -> b)

memoize :: MemoizeableFunction a b -> (a -> Int) -> [a] -> a -> b
memoize f t l i = f' i
  where f' a = [f f' n | n <- l] !! t a

