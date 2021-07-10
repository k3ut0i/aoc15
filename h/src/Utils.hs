module Utils ( printWithPrefix
             , splitOn
             , readLines
             , uninterleave) where

import Data.Functor ((<&>))

printWithPrefix :: (Show a) => String -> a -> IO ()
printWithPrefix s x = putStr s >> print x

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn = s []
  where s :: (Eq a) => [a] -> a -> [a] -> [[a]]
        s acc e (x:xs) | e == x = reverse acc : s [] e xs
                       | otherwise = s (x:acc) e xs
        s acc _ [] = [reverse acc]

readLines :: FilePath -> IO [String]
readLines f = readFile f <&> splitOn '\n'

uninterleave :: [a] -> ([a], [a])
uninterleave [] = ([], [])
uninterleave [x] = ([x], [])
uninterleave (x1:x2:xs) = (x1:xs1, x2:xs2)
  where (xs1, xs2) = uninterleave xs
