module Main where

import Lib
import Utils (printWithPrefix)
import qualified Day1 as D1
import qualified Day2 as D2
import qualified Day3 as D3

main :: IO ()
main = putStrLn "Day1: " >> D1.main >>
       putStrLn "Day2: " >> D2.main >>
       putStrLn "Day3: " >> D3.main
