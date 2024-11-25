module Main where

-- import Data.List
import Data.List.Split

parseTicketValueLine = map read . splitOn ","

parseTicketValues [] = []
parseTicketValues (line:rst) = (parseTicketValueLine line) ++ (parseTicketValues rst)

parseYourTicket (line:rst) = (parseTicketValueLine line, parseTicketValues $ tail $ tail rst)

tuplify [x, y] = (x, y)

-- min-max or min-max -> [(min, max), (min, max)]
expandRule = map (tuplify . map read . splitOn "-") . splitOn " or "

parseFieldLine line = (name, values)
  where (name:rst) = splitOn ": " line
        values = expandRule $ head rst

parseFields fieldRules ("":rst) = (reverse fieldRules, yourTicket, ticketValues)
  where (yourTicket, ticketValues) = parseYourTicket $ tail rst
parseFields fieldRules (line:rst) = parseFields ((parseFieldLine line):fieldRules) rst

checkRule field (min, max) = min <= field && field <= max

checkField fieldRules field = foldl (||) False $ map (checkRule field) $ concat $ map snd fieldRules

calculate (fieldRules, yourTicket, ticketValues) = foldl (+) 0 $ filter (not . checkField fieldRules) ticketValues

main = do
  cnt <- getContents
  print $ calculate $ parseFields [] $ lines cnt

