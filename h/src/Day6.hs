module Day6 ( main,
              lit,
              emptyL,
              update,
              Instr(..)
            ) where

import Text.Read (readPrec, readPrec_to_S)
import Text.ParserCombinators.ReadP
    ( string, ReadP, char, readP_to_S, skipSpaces, (+++) )
import Text.Read.Lex (readDecP)
import Data.Array
import Utils (readLines, printWithPrefix)

data Range = Range (Int, Int) (Int, Int) deriving (Eq)
instance Show Range where
  show (Range (x1, y1) (x2, y2)) =
    show x1 ++ "," ++ show y1 ++ " through " ++
    show x2 ++ "," ++ show y2
instance Read Range where
  readsPrec _ = readP_to_S readRange

readRange :: ReadP Range
readRange = do
  x1 <- readDecP; char ','; y1 <- readDecP
  skipSpaces; string "through"; skipSpaces
  x2 <- readDecP; char ','; y2 <- readDecP
  return $ Range (x1, y1) (x2, y2)

data Op = On | Off | Toggle deriving (Eq)
instance Show Op where
  show On = "turn on"
  show Off = "turn off"
  show Toggle = "toggle"

readOp :: ReadP Op
readOp = (string "turn on" >> return  On) +++
         (string "turn off" >> return Off) +++
         (string "toggle" >> return Toggle)
instance Read Op where
  readsPrec _ = readP_to_S readOp

data Instr = I Op Range deriving (Eq)
instance Show Instr where
  show (I o r) = show o ++ " " ++ show r

readInstr :: ReadP Instr
readInstr = do
  o <- readOp
  skipSpaces
  I o <$> readRange
instance Read Instr where
  readsPrec _ = readP_to_S readInstr

type Lights = Array (Int, Int) Bool
emptyL :: Int -> Lights
emptyL n = array ((0,0), (n-1, n-1)) [((i, j), False) |
                                       i <- [0..n-1],
                                       j <- [0..n-1]]
explodeRange :: Range -> [(Int, Int)]
explodeRange (Range (x1, y1) (x2, y2)) =
  [(i,j) | i <- [x1..x2], j <- [y1..y2]]

update :: Instr -> Lights -> Lights
update (I On r) l =  l // [(i, True) | i <-explodeRange r]
update (I Off r) l = l // [(i, False) | i <- explodeRange r]
update (I Toggle r) l = l // [(i, not (l!i)) | i <- explodeRange r]

lit :: Lights -> Int
lit = foldl (\acc b -> if b then acc+1 else acc) 0

part1 :: [Instr] -> Int
part1 is = lit (foldl (flip update) l is)
  where l = emptyL 1000

type Lights2 = Array (Int, Int) Int
emptyL2 :: Int -> Lights2
emptyL2 n = array ((0,0), (n-1, n-1)) [((i, j),0) |
                                        i <- [0..n-1],
                                        j <- [0..n-1]]
update2 :: Instr -> Lights2 -> Lights2
update2 (I On r) l =  l // [(i, 1+l!i) | i <-explodeRange r]
update2 (I Off r) l = l // [(i, if l!i == 0 then 0 else -1+l!i) | i <- explodeRange r]
update2 (I Toggle r) l = l // [(i, 2+l!i) | i <- explodeRange r]

brightness :: Lights2 -> Int
brightness = sum

part2 :: [Instr] -> Int
part2 is = brightness (foldl (flip update2) l is)
  where l = emptyL2 1000

main :: IO ()
main = (printWithPrefix " part1: " . part1 =<< is) >>
       (printWithPrefix "part2: " . part2 =<< is)
  where
    is :: IO [Instr]
    is = map read <$> readLines "inputs/day6"
