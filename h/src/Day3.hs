module Day3 (main) where

import Data.List (nub)
import Utils (printWithPrefix, uninterleave)

data Direction = U | D | L | R
  deriving (Show, Eq)
instance Read Direction where
  readsPrec _ ('^':s) = [(U, s)]
  readsPrec _ ('v':s) = [(D, s)]
  readsPrec _ ('<':s) = [(L, s)]
  readsPrec _ ('>':s) = [(R, s)]
  readsPrec _ _ = []

readD :: String -> [Direction]
readD = map (\c -> read [c])

step :: (Int, Int) -> Direction -> (Int, Int)
step (x, y) U = (x, y+1)
step (x, y) D = (x, y-1)
step (x, y) L = (x-1, y)
step (x, y) R = (x+1, y)

trail :: (Int, Int) -> [Direction] -> [(Int, Int)]
trail i = foldl (\(p:ps) d -> step p d:p:ps) [i]

part1 :: String -> Int
part1 = length . nub . trail (0, 0) . readD

part2 :: String -> Int
part2 s = length $ nub (t s1 ++ t s2)
  where
    (s1, s2) = uninterleave s
    t = trail (0, 0) . readD
    
main :: IO ()
main = do
  s <- readFile "inputs/day3"
  printWithPrefix " part1: " $ part1 s
  printWithPrefix " part2: " $ part2 s
