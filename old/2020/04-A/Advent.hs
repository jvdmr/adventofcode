module Main where

import Data.List

data Passport = Passport {byr::Bool, iyr::Bool, eyr::Bool, hgt::Bool, hcl::Bool, ecl::Bool, pid::Bool, cid::Bool, err::String}
              deriving Show

defaultPassport = Passport False False False False False False False False ""

valid (Passport byr iyr eyr hgt hcl ecl pid _ "") = byr && iyr && eyr && hgt && hcl && ecl && pid

spaceToNewline ' ' = '\n'
spaceToNewline c = c

splitLinesAndSpaces s = lines $ map spaceToNewline s

parsePassports ps [] = ps
parsePassports ps ("":ls) = parsePassports (defaultPassport:ps) ls
parsePassports (p:ps) (('b':'y':'r':':':_):ls) = parsePassports (p { byr = True }:ps) ls
parsePassports (p:ps) (('i':'y':'r':':':_):ls) = parsePassports (p { iyr = True }:ps) ls
parsePassports (p:ps) (('e':'y':'r':':':_):ls) = parsePassports (p { eyr = True }:ps) ls
parsePassports (p:ps) (('h':'g':'t':':':_):ls) = parsePassports (p { hgt = True }:ps) ls
parsePassports (p:ps) (('h':'c':'l':':':_):ls) = parsePassports (p { hcl = True }:ps) ls
parsePassports (p:ps) (('e':'c':'l':':':_):ls) = parsePassports (p { ecl = True }:ps) ls
parsePassports (p:ps) (('p':'i':'d':':':_):ls) = parsePassports (p { pid = True }:ps) ls
parsePassports (p:ps) (('c':'i':'d':':':_):ls) = parsePassports (p { cid = True }:ps) ls

main = do
  cnt <- getContents
  print $ length $ filter valid $ parsePassports [defaultPassport] $ splitLinesAndSpaces cnt

