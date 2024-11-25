module Main where

-- import Data.List
import Data.List.Split

makeDecks ls = (head decks, head $ tail decks)
  where decks = map (map read . tail) $ splitOn [""] ls

play mem [] b = Right b
play mem a [] = Left a
play mem da@(a:as) db@(b:bs) | elem memKey mem = Left $ a:as
                             | a <= length as && b <= length bs = uncurry (play newMem) $ newDecks subWinner
                             | a < b = play newMem as (bs ++ [b, a])
                             | otherwise = play newMem (as ++ [a, b]) bs
                             where memKey = hashDecks da db
                                   newMem = sort $ memKey:mem
                                   subWinner = play [] (take a as) (take b bs)
                                   newDecks (Left _) = (as ++ [a, b], bs)
                                   newDecks (Right _) = (as, bs ++ [b, a])

hashDecks a b = 1021 * (calculateScore a) + 1031 * (calculateScore b)

calculateScore winner = foldl (+) 0 $ map (uncurry (*)) $ zip [1..] $ reverse winner

unEither (Left x) = x
unEither (Right x) = x

main = do
  cnt <- getContents
  print $ calculateScore $ unEither $ uncurry (play []) $ makeDecks $ lines cnt

