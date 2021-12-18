module Main where

import Data.List
import Data.List.Split (splitOn)

import Text.Parsec
import Text.Parsec.Number (nat)

import Debug.Trace
idtrace x = trace ("debug> " ++ show x) x

data SnailfishNumber = P {lft::SnailfishNumber, rgt::SnailfishNumber}
                     | N {val::Int}
                     deriving (Eq)

instance Show SnailfishNumber where
  show (P l r) = "[" ++ show l ++ "," ++ show r ++ "]"
  show (N n) = show n

brackets = between (char '[') (char ']')

sfn =     (brackets (sfn >>= \l -> char ',' >> sfn >>= return . P l))
      <|> (nat >>= return . N)
      <?> "sfn"

parseNumber = right . parse sfn "(source)"
  where right (Right ast) = ast

addleftmost n (P l r) = P (addleftmost n l) r
addleftmost n (N m) = N (n + m)

addrightmost n (P l r) = P l (addrightmost n r)
addrightmost n (N m) = N (n + m)

explodeD :: Int -> SnailfishNumber -> Maybe (SnailfishNumber, Int, Int)
explodeD 4 (P (N a) (N b)) = Just $ (N 0, a, b)
explodeD d (P l r) = case explodeD (d + 1) l of
                          Just (nl, a, b) -> Just (P nl (addleftmost b r), a, 0)
                          Nothing -> case explodeD (d + 1) r of
                                          Just (nr, a, b) -> Just (P (addrightmost a l) nr, 0, b)
                                          Nothing -> Nothing
explodeD _ n = Nothing

explode :: SnailfishNumber -> Maybe (SnailfishNumber, Int, Int)
explode = explodeD 0

split :: SnailfishNumber -> Maybe SnailfishNumber
split (P l r) = case split l of
                     Just nl -> Just $ P nl r
                     Nothing -> case split r of
                                     Just nr -> Just $ P l nr
                                     Nothing -> Nothing
split (N v) | v >= 10 = Just $ P (N $ div v 2) (N $ div (v + 1) 2)
            | otherwise = Nothing

reduce :: SnailfishNumber -> SnailfishNumber
reduce a = case explode a of
                Just (b, _, _) -> reduce b
                Nothing -> case split a of
                                Just b -> reduce b
                                Nothing -> a

add :: SnailfishNumber -> SnailfishNumber -> SnailfishNumber
add a b = reduce $ P a b

addsfn :: [SnailfishNumber] -> SnailfishNumber
addsfn (a:rst) = foldl add a rst

magnitude :: SnailfishNumber -> Int
magnitude (P l r) = 3 * magnitude l + 2 * magnitude r
magnitude (N v) = v

main = do
  cnt <- getContents
  print $ map (magnitude . idtrace . addsfn . map parseNumber) $ splitOn [""] $ lines cnt

