{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module AoC2023.Day24
  ( part1
  , part2
  ) where

import Data.List.Split (splitOn)

import AoC (Solver)
import AoC.Util (between)

parseCoord :: String -> (Double, Double, Double)
parseCoord s = read $ "(" ++ s ++ ")"

type Position = (Double, Double, Double)
type Vector = (Double, Double, Double)
type Hail = (Position, Vector)

hail :: String -> Hail
hail s = (p, v)
  where [p, v] = map parseCoord $ splitOn " @ " s
  
parallelXY :: Hail -> Hail -> Bool
parallelXY (_, (vax, vay, _)) (_, (vbx, vby, _)) = 0 == (vby * vax) - (vbx * vay)

--   { pax + a * vax == pbx + b * vbx
--   { pay + a * vay == pby + b * vby
-- <=> (pbx - pax + (b * vbx)) / vax == (pby - pay + (b * vby)) / vay
-- <=> (pbx - pax) / vax + b * (vbx / vax) == (pby - pay) / vay + b * (vby / vay)
-- <=> (pbx - pax) / vax - (pby - pay) / vay == b * (vby / vay) - b * (vbx / vax)
-- <=> (pbx - pax) / vax - (pby - pay) / vay == b * ((vby / vay) - (vbx / vax))
-- <=> ((pbx - pax) / vax - (pby - pay) / vay) / ((vby / vay) - (vbx / vax)) == b

futureIntersectXY :: Hail -> Hail -> Maybe Position
futureIntersectXY ((pax, pay, _), (vax, vay, _)) ((pbx, pby, _), (vbx, vby, _))
  | a < 0 || b < 0 = Nothing
  | otherwise = Just (pax + a * vax, pay + a * vay, 0)
  where b = ((pbx - pax) / vax - (pby - pay) / vay) / ((vby / vay) - (vbx / vax))
        a = (pbx - pax + (b * vbx)) / vax

type Range = (Double, Double)

testRange :: Range
testRange = (7, 27)

prodRange :: Range
prodRange = (200000000000000, 400000000000000)

range :: [a] -> Range
range hs | length hs > 10 = prodRange
         | otherwise = testRange

inRange :: Range -> Maybe Position -> Bool
inRange (s, e) Nothing = False
inRange (s, e) (Just (x, y, z)) = all (between s e) [x, y]

combine :: [a] -> [(a, a)]
combine [] = []
combine (a:rest) = (zip (repeat a) rest) ++ combine rest

part1 :: Solver
part1 = show . length . inRange' . map (uncurry futureIntersectXY) . filter (not . uncurry parallelXY) . combine . map hail . lines
  where inRange' l = filter (inRange $ range l) l

part2 :: Solver
part2 = show . length . lines

