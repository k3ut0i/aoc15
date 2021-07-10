module Utils (printWithPrefix) where

printWithPrefix :: (Show a) => String -> a -> IO ()
printWithPrefix s x = putStr s >> print x
