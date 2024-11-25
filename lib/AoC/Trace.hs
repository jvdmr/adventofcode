{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC.Trace
  ( idtrace
  , ftrace
  , rtrace
  ) where

import Debug.Trace (trace)

idtrace :: (Show a) => a -> a
idtrace x = trace (show x) x

ftrace :: (a -> String) -> a -> a
ftrace f x = trace (f x) x

rtrace :: (Show a, Show b) => (a -> b) -> a -> b
rtrace f n = ftrace (\r -> show n ++ " => " ++ show r) $ f n

