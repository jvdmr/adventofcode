module Main where

-- since we're only working with straight angles, I'll make my own cos and sin. With blackjack! And hookers!
import Prelude hiding (Left, Right, cos, sin)
-- import Data.List

cos 0 = 1
cos 90 = 0
cos 180 = -1
cos 270 = 0

sin 0 = 0
sin 90 = 1
sin 180 = 0
sin 270 = -1

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

turn dir val = mod (dir+val) 360

move (x, y, dir) (North val) = (x, (y+val), dir)
move (x, y, dir) (South val) = (x, (y-val), dir)
move (x, y, dir) (East val) = ((x+val), y, dir)
move (x, y, dir) (West val) = ((x-val), y, dir)
move (x, y, dir) (Left val) = (x, y, (turn dir val))
move (x, y, dir) (Right val) = (x, y, (turn dir (-val)))
move (x, y, dir) (Forward val) = ((x+(cos dir)*val), (y+(sin dir)*val), dir)

start = (0, 0, 0)

distance (x, y, _) = (abs x) + (abs y)

main = do
  cnt <- getContents
  print $ distance $ foldl move start $ map parseIns $ lines cnt

