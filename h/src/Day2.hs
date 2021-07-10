module Day2 (main) where

import Utils (splitOn, printWithPrefix, readLines)
import Data.Functor ((<&>))

newtype RCuboid = R (Int, Int, Int) deriving (Eq, Show)
instance Read RCuboid where
  readsPrec _ s =
    let [l, w, h] = map read (splitOn 'x' s) in
      [(R (l, w, h), "")]

surfaceArea :: RCuboid -> Int
surfaceArea (R (l, w, h)) = minimum sizeareas + 2 * sum sizeareas
  where
    sizeareas = [l*w, w*h, h*l]

ribbonSize :: RCuboid -> Int
ribbonSize (R (l, w, h)) = 2 * minimum [l+w, w+h, h+l]

volume :: RCuboid -> Int
volume (R (l, w, h)) = l*w*h

part1 :: [String] -> IO ()
part1 lines = printWithPrefix " part1: " xs
  where
    xs = sum (map (surfaceArea . read) lines)

part2 :: [String] -> IO ()
part2 lines = printWithPrefix " part2: " xs
  where
    xs = sum (map ((\c -> volume c + ribbonSize c) . read) lines)

main :: IO ()
main = do
  lines <- readLines "inputs/day2"
  part1 lines
  part2 lines
