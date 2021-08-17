module Utils ( printWithPrefix
             , splitOn
             , readLines
             , uninterleave
             , sumWith) where


printWithPrefix :: (Show a) => String -> a -> IO ()
printWithPrefix s x = putStr s >> print x

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn = s ([], [])
  where s :: (Eq a) => ([a], [[a]]) -> a -> [a] -> [[a]]
        s (acc, as) e [x] | e == x = reverse $ reverse acc:as-- skip empty list if e is the last element
        s (acc, as) e (x:xs) | e == x = s ([], reverse acc:as) e xs
                             | otherwise = s (x:acc, as) e xs
        s (acc, as) _ [] = reverse $ reverse acc:as

readLines :: FilePath -> IO [String]
readLines f =  splitOn '\n' <$> readFile f

uninterleave :: [a] -> ([a], [a])
uninterleave [] = ([], [])
uninterleave [x] = ([x], [])
uninterleave (x1:x2:xs) = (x1:xs1, x2:xs2)
  where (xs1, xs2) = uninterleave xs

sumWith :: (Foldable t, Num b) => (a -> b) -> t a -> b
sumWith f = foldr (\s a -> a + f s) 0
