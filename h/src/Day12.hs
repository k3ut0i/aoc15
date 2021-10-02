module Day12 () where

import Data.Char (ord)
import Utils (printWithPrefix)
import Data.Aeson
import Data.Scientific (toBoundedInteger)
import Data.ByteString.Lazy.Char8 (pack)
--import Data.HashMap (foldl')
import qualified Data.Text as T (unpack)

first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

isDigit :: Char -> Bool
isDigit x = ord x >= 48 && ord x <= 57

isMinus :: Char -> Bool
isMinus x = ord x == 45

getNum :: String -> Bool -> Maybe (Int, String)
getNum [] _ = Nothing
getNum s@(x:xs) sign
  | isDigit x = Just $ first (\i -> i * (if sign then -1 else 1)) $ slurpDigits s
  | isMinus x = getNum xs True
  | otherwise = getNum xs False
  where
    slurpDigits :: String -> (Int, String)
    slurpDigits = first read . span isDigit

getSum :: String -> Int -> Int
getSum str acc = case getNum str False of
                   Just (n, r) -> getSum r (acc + n)
                   Nothing -> acc

isRed :: Object -> Bool
isRed = foldr (\v acc -> acc || f v) False
  where
    f v = case v of
            (String s) -> T.unpack s == "red" -- why doesn't `show s == "red"` work?
            _ -> False
      
calcScore :: Value -> Int
calcScore Null = 0
calcScore (Bool _) = 0
calcScore (String _) = 0
calcScore (Number n) = case toBoundedInteger n of
                         Nothing ->
                           error $ "Found unqualified number: " ++ show n
                         Just i -> i
calcScore (Array ar) = foldl (\acc v -> acc + calcScore v) 0 ar
calcScore (Object hm) | isRed hm = 0
                      | otherwise = foldl (\acc v -> acc + calcScore v) 0 hm

part2 :: String -> IO Int
part2 s = case decode $ pack s of
            Nothing -> error "Could not parse the JSON string"
            Just jo -> return $ calcScore jo

main :: IO ()
main =  do
  s <- readFile "inputs/day12"
  printWithPrefix " part1: " (getSum s 0)
  printWithPrefix " part2: " =<< part2 s
