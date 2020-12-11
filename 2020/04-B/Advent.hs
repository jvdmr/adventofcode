module Main where

import Data.List
import Data.Char

between a b c | a <= b = b <= c
              | otherwise = False

data Passport = Passport {byr::Bool, iyr::Bool, eyr::Bool, hgt::Bool, hcl::Bool, ecl::Bool, pid::Bool, cid::Bool, err::String}
              deriving Show

defaultPassport = Passport False False False False False False False False ""

valid (Passport byr iyr eyr hgt hcl ecl pid _ "") = byr && iyr && eyr && hgt && hcl && ecl && pid

spaceToNewline ' ' = '\n'
spaceToNewline c = c

splitLinesAndSpaces s = lines $ map spaceToNewline s

validBYR val = between 1920 (read val) 2002

validIYR val = between 2010 (read val) 2020

validEYR val = between 2020 (read val) 2030

validHGT val | dropWhile isDigit val == "in" = between 59 (read $ takeWhile isDigit val) 76
             | dropWhile isDigit val == "cm" = between 150 (read $ takeWhile isDigit val) 193
             | otherwise = False

validHCL ('#':cs) = (length $ filter isHexDigit cs) == 6
validHCL _ = False

validECL "amb" = True
validECL "blu" = True
validECL "brn" = True
validECL "gry" = True
validECL "grn" = True
validECL "hzl" = True
validECL "oth" = True
validECL _ = False

validPID val = (length $ filter isDigit val) == 9 

validCID _ = True

parsePassports ps [] = ps
parsePassports ps ("":ls) = parsePassports (defaultPassport:ps) ls
parsePassports (p:ps) (('b':'y':'r':':':val):ls) = parsePassports (p { byr = validBYR val }:ps) ls
parsePassports (p:ps) (('i':'y':'r':':':val):ls) = parsePassports (p { iyr = validIYR val }:ps) ls
parsePassports (p:ps) (('e':'y':'r':':':val):ls) = parsePassports (p { eyr = validEYR val }:ps) ls
parsePassports (p:ps) (('h':'g':'t':':':val):ls) = parsePassports (p { hgt = validHGT val }:ps) ls
parsePassports (p:ps) (('h':'c':'l':':':val):ls) = parsePassports (p { hcl = validHCL val }:ps) ls
parsePassports (p:ps) (('e':'c':'l':':':val):ls) = parsePassports (p { ecl = validECL val }:ps) ls
parsePassports (p:ps) (('p':'i':'d':':':val):ls) = parsePassports (p { pid = validPID val }:ps) ls
parsePassports (p:ps) (('c':'i':'d':':':val):ls) = parsePassports (p { cid = validCID val }:ps) ls

main = do
  cnt <- getContents
  print $ length $ filter valid $ parsePassports [defaultPassport] $ splitLinesAndSpaces cnt

