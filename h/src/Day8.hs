module Day8 (main) where

import Utils (readLines, printWithPrefix, sumWith)

main :: IO ()
main = do
  l <- readLines "inputs/day8"
  let originalLength = sum (map length l)
  printWithPrefix " part1: " (originalLength - sumWith countChar l)
  printWithPrefix " part2: " (sumWith encodeCount l - originalLength)

countChars :: Int -> [Char] -> Int
countChars acc ('\\':'"':cs) = countChars (acc+1) cs
countChars acc ('\\':'\\':cs) = countChars (acc+1) cs
countChars acc ('\\':'x':_:_:cs) = countChars (acc+1) cs
countChars acc ['"'] = acc
countChars acc (_:cs) = countChars (acc+1) cs
countChars acc cs = error $ "Unknown string pattern: " ++ cs

countChar :: [Char] -> Int
countChar ('"':cs) = countChars 0 cs
countChar [] = 0
countChar cs = error $ "String does not start with double-quotes: " ++ cs

encodeCount' :: Int -> [Char] -> Int
encodeCount' acc ('"':cs) = encodeCount' (acc+2) cs
encodeCount' acc ('\\':cs) = encodeCount' (acc+2) cs
encodeCount' acc (_:cs) = encodeCount' (acc+1) cs
encodeCount' acc "" = acc

encodeCount :: String -> Int
encodeCount s = 2 + encodeCount' 0 s
