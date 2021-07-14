module Main where

import Lib
import Utils (printWithPrefix)
import qualified Day1 as D1
import qualified Day2 as D2
import qualified Day3 as D3
import qualified Day5 as D5
import qualified Day6 as D6

main :: IO ()
main = putStrLn "Day1: " >> D1.main >>
       putStrLn "Day2: " >> D2.main >>
       putStrLn "Day3: " >> D3.main >>
       putStrLn "Day5: " >> D5.main >>
       putStrLn "Day6: " >> D6.main
