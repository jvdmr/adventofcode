{-# LANGUAGE FlexibleContexts, LambdaCase #-}
module Main where

import Control.Monad (foldM_, zipWithM_)
import Control.Monad.ST (ST, runST)
import Data.Array.ST (Ix, MArray, STUArray, getBounds, newArray_, readArray, writeArray)
import Data.Char (digitToInt, isDigit)
import Data.List ((\\))

step :: (MArray a e m, Enum e, Ix e) => a e e -> e -> m e
step arr x = do
  (lo, hi) <- getBounds arr
  a <- readArray arr x
  b <- readArray arr a
  c <- readArray arr b
  y <- readArray arr c
  let pred' z = if z == lo then hi else pred z
      t = until (`notElem` [a, b, c]) pred' $ pred' x
  u <- readArray arr t
  writeArray arr x y
  writeArray arr t a
  writeArray arr c u
  pure y

newArray' :: [Int] -> ST s (STUArray s Int Int)
newArray' xs = do
  arr <- newArray_ (minimum xs, maximum xs)
  arr <$ zipWithM_ (writeArray arr) xs (drop 1 $ cycle xs)

day23b :: String -> Int
day23b input = runST $ do
  let nums@(x:_) = digitToInt <$> filter isDigit input
  arr <- newArray' $ nums ++ ([1..1000000] \\ nums)
  foldM_ (const . step arr) x $ replicate 10000000 ()
  y <- readArray arr 1
  z <- readArray arr y
  pure $ y * z

main = do
  cnt <- getContents
  print $ day23b cnt
