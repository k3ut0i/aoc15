module Day5 ( main
            , nice
            , pairRep
            , skipRep) where

import Data.List (isInfixOf)
import Data.Function ((&))
import Utils (printWithPrefix, readLines)
import Control.Applicative (liftA2)

vowels :: String -> String
vowels = filter (`elem` "aeiou")

twiceRep :: String -> Bool
twiceRep "" = False
twiceRep [_] = False
twiceRep (x1:x2:xs) | x1 == x2 = True
                    | otherwise = twiceRep (x2:xs)

nice :: String -> Bool
nice s = not (any (`isInfixOf` s) ["ab", "cd", "pq", "xy"]) &&
         length (vowels s) >= 3 &&
         twiceRep s

part1 :: [String] -> Int
part1 = length . filter nice

pairRep :: String -> Bool
pairRep "" = False
pairRep [_] = False
pairRep (x:y:xs) = [x,y] `isInfixOf` xs ||
                   pairRep (y:xs)

skipRep :: String -> Bool
skipRep "" = False
skipRep [_] = False
skipRep [_, _] = False
skipRep (x1:x2:x3:xs) | x1 == x3 = True
                      | otherwise = skipRep (x2:x3:xs)
                      
nice2 :: String -> Bool
nice2 = liftA2 (&&) pairRep skipRep
  -- found this when looking at applicative instance for ((->) r)
  -- liftA2 q f g x = q (f x) (g x)

part2 :: [String] -> Int
part2 = length . filter nice2

main :: IO ()
main = do
  lines <- readLines "inputs/day5"
  printWithPrefix " part1: " $ part1 lines
  printWithPrefix " part2: " $ part2 lines
  
  
