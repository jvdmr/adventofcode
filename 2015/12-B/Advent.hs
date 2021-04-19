module Main where

import Data.List hiding (insert)
import Data.Ratio
import Text.JSON

import Debug.Trace
idtrace x = trace (show x) x

seekAndSum :: JSValue -> Integer
seekAndSum (JSArray a) = foldl (+) 0 $ map seekAndSum a
seekAndSum o@(JSObject a) | rejectRed o = 0
                          | otherwise = foldl (+) 0 $ map (seekAndSum . snd) $ fromJSObject a
seekAndSum (JSRational _ n) = numerator n
seekAndSum _ = 0

rejectRed :: JSValue -> Bool
rejectRed (JSObject a) = elem "red" $ map (extractStrings . snd) $ fromJSObject a

extractStrings :: JSValue -> String
extractStrings (JSString s) = fromJSString s
extractStrings _ = ""

getResult :: Result a -> a
getResult (Ok result) = result

main = do
  cnt <- getContents
  print $ map (seekAndSum . getResult . decode) $ lines cnt

