{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC.Memoize
  ( MemoizableFunction (..)
  , memoize
  ) where

import Data.Function (fix)

type MemoizableFunction b = (Int -> b) -> (Int -> b)

memoize' :: (Int -> b) -> (Int -> b)
memoize' f = (map f [0..] !!)

memoize :: MemoizableFunction b -> (Int -> b)
memoize f = fix (memoize' . f)

