{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC.Trace
  ( trace
  , ftrace
  , idtrace
  , sidtrace
  , sftrace
  , rftrace
  , rftrace2
  , srftrace
  , rtrace
  , rtrace2
  , showCGrid
  , showSGrid
  , showCGrids
  , showGrid
  , showGrids
  ) where

import Data.List (intercalate, transpose)
import Debug.Trace (trace)

showGrid :: Show a => String -> [[a]] -> String
showGrid s = (++) "\n" . flip (++) "\n" . intercalate "\n" . map (intercalate s . map show)

showCGrid :: [[Char]] -> String
showCGrid = (++) "\n" . flip (++) "\n" . intercalate "\n"

showSGrid :: String -> [[String]] -> String
showSGrid s = (++) "\n" . flip (++) "\n" . intercalate "\n" . map (intercalate s)

showGrids :: Show a => [[[a]]] -> String
showGrids gs = showCGrid [intercalate "    " $ map show z | z <- transpose gs]

showCGrids :: [[[Char]]] -> String
showCGrids gs = showCGrid [intercalate "    " z | z <- transpose gs]

ftrace :: (a -> String) -> a -> a
ftrace f x = trace (f x) x

idtrace :: (Show a) => a -> a
idtrace = ftrace show

sidtrace :: (Show a) => String -> a -> a
sidtrace s = ftrace ((++) s . show)

sftrace :: Show sa => (a -> sa) -> a -> a
sftrace f = ftrace $ show . f

rftrace :: (a -> String) -> (b -> String) -> (a -> b) -> a -> b
rftrace showa showb f n = ftrace (\r -> showa n ++ " -> " ++ showb r) $ f n

rftrace2 :: (a -> String) -> (b -> String) -> (c -> String) -> (a -> b -> c) -> a -> b -> c
rftrace2 showa showb showc f m n = ftrace (\r -> showa m ++ " -> " ++ showb n ++ " -> " ++ showc r) $ f m n

srftrace :: (Show sa, Show sb) => (a -> sa) -> (b -> sb) -> (a -> b) -> a -> b
srftrace fa fb = rftrace (show . fa) (show . fb)

rtrace :: (Show a, Show b) => (a -> b) -> a -> b
rtrace = rftrace show show

rtrace2 :: (Show a, Show b, Show c) => (a -> b -> c) -> a -> b -> c
rtrace2 = rftrace2 show show show

