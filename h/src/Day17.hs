module Day17 () where

import Utils (readLines, printWithPrefix)

readData :: FilePath -> IO [Int]
readData f = map read <$> readLines f

combinations :: [a] -> [[a]]
combinations [] = [[]]
combinations (x:xs) = let c = combinations xs in
                        c ++ map (x:) c

fullp :: [Int] -> Int -> Bool
fullp xs s = s == sum xs

possible :: [Int] -> Int
possible = length . filter (`fullp` 150) . combinations

minContainers :: [Int] -> Int
minContainers ss = length $ filter (\l -> length l == m) p
  where p = filter (`fullp` 150) $ combinations ss
        m = minimum . map length $ p

main :: IO ()
main = readData "inputs/day17" >>=
       \d -> printWithPrefix " part1: " (possible d) >>
       printWithPrefix " part2: " (minContainers d)

