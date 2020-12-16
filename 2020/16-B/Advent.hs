module Main where

import Debug.Trace
import Data.List
import Data.List.Split

lbltrace s x = trace (s ++ show x) x

data Field = Departure {name::String, rules::[(Int, Int)]}
           | Arrival {name::String, rules::[(Int, Int)]}
           | Other {name::String, rules::[(Int, Int)]}
           deriving (Eq, Ord)

instance Show Field where
  show (Departure name _) = "departure " ++ name
  show (Arrival name _) = "arrival " ++ name
  show (Other name _) = name

--         --
--  parse  --
--         --

parseTicketValueLine = map read . splitOn ","

parseTicketValues [] = []
parseTicketValues (line:rst) = (parseTicketValueLine line):(parseTicketValues rst)

parseYourTicket (line:rst) = (parseTicketValueLine line, parseTicketValues $ tail $ tail rst)

tuplify [x, y] = (x, y)

-- min-max or min-max -> [(min, max), (min, max)]
expandRule = map (tuplify . map read . splitOn "-") . splitOn " or "

field ["departure", name] = Departure name
field ["arrival", name] = Arrival name
field [name] = Other name

parseFieldLine line = field (splitOn " " name) rules
  where (name:rst) = splitOn ": " line
        rules = expandRule $ head rst

parseFields fields ("":rst) = (sort fields, yourTicket, ticketValues)
  where (yourTicket, ticketValues) = parseYourTicket $ tail rst
parseFields fields (line:rst) = parseFields ((parseFieldLine line):fields) rst

--         --
--  solve  --
--         --

checkRule value (min, max) = min <= value && value <= max

flatten [] = []
flatten ((a:rsta):rst) = a:(flatten (rsta:rst))
flatten ([]:rst) = flatten rst

checkValue fields value = foldr (||) False $ map (checkRule value) $ flatten $ map rules fields

checkValid fields ticket = foldr (&&) True $ map (checkValue fields) ticket

checkField field value = foldr (||) False $ map (checkRule value) $ rules field

checkFieldValues values field = foldr (&&) True $ map (checkField field) values

transposeTicketValues :: [[Int]] -> [[Int]]
transposeTicketValues [] = []
transposeTicketValues ([]:_) = []
transposeTicketValues tickets = (map head tickets):transposeTicketValues (map tail tickets)

filterField :: Eq a => [[a]] -> a -> [[a]]
filterField possibleFields field = map removeUnlessSingleton possibleFields
  where removeUnlessSingleton fields | length fields == 1 = fields
                                     | otherwise = filter (/= field) fields

uniqueFields :: Ord a => [a] -> Bool
uniqueFields = findUnique . sort
  where findUnique [a] = True
        findUnique (a:b:rst) | a == b = False
                             | otherwise = findUnique (b:rst)

allSingletons :: [[a]] -> Bool
allSingletons = (== 0) . length . filter (/= 1) . map length

getSingletons :: [[a]] -> [a]
getSingletons = map head . filter ((== 1) . length)

reduce :: [[Field]] -> [[Field]]
reduce possibleFields | uniqueFields possibleFields && allSingletons possibleFields = possibleFields
                      | otherwise = reduce nextStep
                      where nextStep = foldl filterField possibleFields singletons
                            singletons = getSingletons possibleFields

findFieldOrder :: [Field] -> [[Int]] -> [Field]
findFieldOrder fields tickets = map head $ reduce $ map (\values -> filter (checkFieldValues values) fields) $ transposeTicketValues tickets

--          --
-- checksum --
--          --

isDeparture ((Departure _ _), value) = value
isDeparture _ = 1

calculate (fields, yourTicket, tickets) = foldl (*) 1 $ map isDeparture $ lbltrace "ordered fields: " $ zip orderedFields yourTicket
  where orderedFields = findFieldOrder fields $ yourTicket:validTickets
        validTickets = filter (checkValid fields) tickets

main = do
  cnt <- getContents
  print $ calculate $ parseFields [] $ lines cnt

