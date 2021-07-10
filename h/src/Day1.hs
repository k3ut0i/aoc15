module Day1 ( main
            , countFloors
            , findBasement) where

import Utils

score :: Char -> Int
score '(' = 1
score ')' = -1
score c = error $ "Unknown Character: " ++ [c, '\n']

countFloors :: String -> Int
countFloors = foldl (\acc c -> acc + score c) 0

findBasement :: (Int, Int) -> String -> Maybe Int
findBasement (pos, idx) (c:cs)
  | newpos == -1 = Just newidx
  | otherwise = findBasement (newpos, newidx) cs
  where
    newpos = pos + score c
    newidx = idx + 1
findBasement _ [] = Nothing

part1 :: String -> IO()
part1 s = printWithPrefix "  part1: " (countFloors s)

part2 :: String -> IO()
part2 s = printWithPrefix "  part2: " (findBasement (0, 0) s)
          
main :: IO()
main =  sio >>= part1 >>
        sio >>= part2
  where
    sio = readFile "inputs/day1"
    
