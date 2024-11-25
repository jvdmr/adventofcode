{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC.Trace
  ( idtrace
  , ftrace
  ) where

import Debug.Trace (trace)

idtrace :: (Show a) => a -> a
idtrace x = trace (show x) x

ftrace :: (a -> String) -> a -> a
ftrace f x = trace (f x) x

