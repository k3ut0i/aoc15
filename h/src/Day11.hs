module Day11 (goodPass, findNext, main) where
import Utils (printWithPrefix)
import Data.List (nub)
import Data.Char (ord, chr) 
{-
I am defining this in reverse order because of list representation
-}

inc :: [Int] -> [Int]
inc [] = []
inc (x:xs) | x < 25 = (x+1):xs
           | x == 25 = 0 : inc xs
           | otherwise = error "Out of bounds index for an alphabet"     

-- decreasing subseqence      
straight3 :: [Int] -> Bool
straight3 [] = False
straight3 [_] = False
straight3 [_, _] = False
straight3 (x1:x2:x3:xs) | (x1 == (x2 + 1)) && (x2 == (x3 + 1)) = True
                        | otherwise = straight3 (x2:x3:xs)

noIOL :: [Int] -> Bool
noIOL = not . any (\c -> c == 8 || c == 11 || c == 14)

twice :: [Int] -> Maybe (Int, [Int])
twice [] = Nothing
twice [_] = Nothing
twice (x1:x2:xs) | x1 == x2 = Just (x1, xs)
                 | otherwise = twice (x2:xs)

all2s :: [Int] -> [Int]
all2s xs = case twice xs of
             Nothing -> []
             Just (x,r) -> x:all2s r

double2s :: [Int] -> Bool
double2s xs = case nub (all2s xs) of
                (_:_:_) -> True -- atleast two unique elements
                _ -> False

goodPassI :: [Int] -> Bool
goodPassI xs = double2s xs && noIOL xs && straight3 xs

strToInt :: String -> [Int]
strToInt = reverse . map (\c -> -97 + ord c)

intToStr :: [Int] -> String
intToStr = map (\i -> chr $ i + 97) . reverse

goodPass :: String -> Bool
goodPass s = goodPassI l
  where
    l = reverse $ map (\c -> -97 + ord c) s

findNext :: String -> String
findNext s = intToStr $ until goodPassI inc (strToInt s)

main :: IO ()
main = printWithPrefix " part1: " p1 >>
       printWithPrefix " part2: " p2
  where
    p1 = findNext "vzbxkghb"
    p2 = findNext . intToStr . inc . strToInt $ p1
