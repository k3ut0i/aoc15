module Day4 (main) where

import Crypto.Hash.MD5 (hash)
import Data.ByteString.Char8 (pack, unpack)
import Text.Printf (printf)
import Utils (printWithPrefix)

hex :: String -> String
hex = foldl (\acc c -> acc ++ hc c) ""
  where
    hc :: Char -> [Char]
    hc c = printf "%.02x" c

md5hex :: String -> String
md5hex = hex . unpack. hash . pack

n :: String -> Int -> Int -> Bool
n key nzeros i =
  take nzeros (md5hex (key ++ show i)) == replicate nzeros '0'

findN :: String -> Int -> Int
findN key nz = until (n key nz) (+1) 0

main :: IO ()
main = printWithPrefix " part1: " (findN s 5) >>
       printWithPrefix " part2: " (findN s 6)
  where s = "ckczppom"
