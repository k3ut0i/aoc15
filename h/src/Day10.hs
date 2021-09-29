module Day10 (lookAndSay, main) where
import Utils (nTimes, printWithPrefix)

collapse' :: Eq a => [a] -> (Int, a) -> [(Int, a)] -> [(Int, a)]
collapse' [] f a = reverse (f:a)
collapse' (x:xs) (count, current) acc
  | x == current = collapse' xs (1+count, current) acc
  | otherwise = collapse' xs (1, x) ((count, current):acc)

lookAndSay :: String -> String
lookAndSay [] = []
lookAndSay (x:xs) = collect (collapse' xs (1, x) [])
  where
    collect :: [(Int, Char)] -> String
    collect [] = []
    collect ((x1, x2):xs) = show x1 ++ x2:collect xs

main :: IO ()
main =  printWithPrefix " part1: " (length n40) >>
        printWithPrefix " part2: " (length n50)
  where
    n40 = nTimes 40 lookAndSay "1113122113"
    n50 = nTimes 10 lookAndSay n40
       
