{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC.Memoize
  ( MemoizableFunction (..)
  , memoize
  ) where

import Data.Map

import Vdmr.Trace

type MemoizableFunction a b = ((a -> b) -> a -> b)

memoize :: (Ord a) => MemoizableFunction a b -> [a] -> (a -> b)
memoize f l = f!
  where mem = fromList [(n, f f' n) | n <- l]
        f' a = mem ! a

