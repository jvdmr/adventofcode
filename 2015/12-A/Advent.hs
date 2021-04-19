module Main where

import Data.List hiding (insert)
import Data.Ratio
import Text.JSON

import Debug.Trace
idtrace x = trace (show x) x

seekAndSum :: JSValue -> Integer
seekAndSum (JSArray a) = foldl (+) 0 $ map seekAndSum a
seekAndSum (JSObject a) = foldl (+) 0 $ map (seekAndSum . snd) $ fromJSObject a
seekAndSum (JSRational _ n) = numerator n
seekAndSum _ = 0

getResult :: Result a -> a
getResult (Ok result) = result

main = do
  cnt <- getContents
  print $ map (seekAndSum . getResult . decode) $ lines cnt

