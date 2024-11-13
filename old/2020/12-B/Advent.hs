module Main where

import Prelude hiding (Left, Right)
import Data.List

data Instruction = North {value::Int}
                 | South {value::Int}
                 | East {value::Int}
                 | West {value::Int}
                 | Left {value::Int}
                 | Right {value::Int}
                 | Forward {value::Int}
                 deriving (Show, Eq)

parseIns ('N':val) = North $ read val
parseIns ('S':val) = South $ read val
parseIns ('E':val) = East $ read val
parseIns ('W':val) = West $ read val
parseIns ('L':val) = Left $ read val
parseIns ('R':val) = Right $ read val
parseIns ('F':val) = Forward $ read val

turn (x, y) 90 = (-y, x)
turn (x, y) 180 = (-x, -y)
turn (x, y) 270 = (y, -x)
turn (x, y) val = turn (x, y) $ mod val 360

move (x, y, (wpx, wpy)) (North val) = (x, y, (wpx, wpy+val))
move (x, y, (wpx, wpy)) (South val) = (x, y, (wpx, wpy-val))
move (x, y, (wpx, wpy)) (East val) = (x, y, (wpx+val, wpy))
move (x, y, (wpx, wpy)) (West val) = (x, y, (wpx-val, wpy))

move (x, y, (wpx, wpy)) (Left val) = (x, y, (turn (wpx, wpy) val))
move (x, y, (wpx, wpy)) (Right val) = (x, y, (turn (wpx, wpy) (-val)))

move (x, y, (wpx, wpy)) (Forward val) = (x+wpx*val, y+wpy*val, (wpx, wpy))

start = (0, 0, (10, 1))

distance (x, y, _) = (abs x) + (abs y)

main = do
  cnt <- getContents
  print $ distance $ foldl move start $ map parseIns $ lines cnt

