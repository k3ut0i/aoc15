module Utils ( printWithPrefix
             , splitOn
             , readLines
             , uninterleave
             , sumWith
             , nTimes
             , Identity) where


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

nTimes :: Int -> (a -> a) -> a -> a
nTimes n f x | n == 0 = x
             | n > 0  = nTimes (n-1) f (f x)
             | otherwise = error "the number of times a function must be applied should be positive"

-- there seems to be a permutations in Data.List             
permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) = concatMap (f x) $ permutations xs
  where f :: a -> [a] -> [[a]]
        f x [] = [[x]]
        f x (y:ys) = (x:y:ys) : map (y:) (f x ys)

newtype Identity a = Identity {runIdentity :: a}

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity x) = Identity (f x)
  
instance Monad Identity where
  (Identity x) >>= f = f x
