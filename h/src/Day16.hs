module Day16 ( scannerData
             , Poss
             , isSubset
             , Obj(..)) where

import Text.ParserCombinators.ReadP (ReadP, string, readS_to_P, char,
                                     readP_to_S, skipSpaces, (+++),
                                     sepBy, eof)
import Text.Read.Lex (hsLex, readDecP)
import Utils (readLines, printWithPrefix)
import Data.HashMap.Strict ( HashMap, fromList, foldrWithKey
                           , (!), keys, lookupDefault)
import qualified Data.HashMap.Strict as HM (filter)
import Data.Hashable

data Obj = Children | Cats | Goldfish | Trees | Cars | Perfumes
         | Samoyeds | Pomeranians | Akitas | Vizslas
  deriving (Eq, Show)

instance Hashable Obj where
  hashWithSalt s o = hashWithSalt s (show o)

type Poss = HashMap Obj Int -- possessions Objs of quantity Int
type Aunts = HashMap Int Poss -- aunt number and her possessions

{-
strToObj :: String -> Maybe Obj
strToObj "children" =  Just Children
strToObj "cats" = Just Cats
strToObj "goldfish" = Just Goldfish
strToObj "trees" = Just Trees
strToObj "cars" = Just Cars
strToObj "perfumes" = Just Perfumes
strToObj "samoyeds" = Just Samoyeds
strToObj "pomeranians" = Just Pomeranians
strToObj "akitas" = Just Akitas
strToObj "vizslas" = Just Vizslas
strToObj _ = Nothing

readObj :: ReadP Obj
readObj = do
  s <- hsLex
  maybe pfail return (strToObj s)
-}

readObj :: ReadP Obj
readObj = (string "children" >> return Children)
          +++ (string "cats" >> return Cats)
          +++ (string "goldfish" >> return Goldfish)
          +++ (string "trees" >> return Trees)
          +++ (string "cars" >> return Cars)
          +++ (string "perfumes" >> return Perfumes)
          +++ (string "samoyeds" >> return Samoyeds)
          +++ (string "pomeranians" >> return Pomeranians)
          +++ (string "akitas" >> return Akitas)
          +++ (string "vizslas" >> return Vizslas)


readPair :: ReadP (Obj, Int)
readPair = do
  obj <- readObj
  char ':' >> skipSpaces
  (,) obj <$> readDecP

readPoss :: ReadP Poss
readPoss =
  fromList <$> sepBy readPair (skipSpaces >> char ',' >> skipSpaces)

readAuntP :: ReadP (Int, Poss)
readAuntP = do
  string "Sue "
  n <- readDecP
  string ": "
  p <- readPoss
  eof -- to make sure that readPoss actually reads all the possessions
  return (n, p)

readAunt :: String -> (Int, Poss)
readAunt s = case readP_to_S readAuntP s of
               [(e, [])] -> e
               _ -> error $ "parse error of aunt entry" ++ s

readData :: FilePath -> IO Aunts
readData f = fromList . map readAunt <$> readLines f

scannerData :: Poss
scannerData = fromList [ (Children, 3)
                       , (Cats, 7)
                       , (Samoyeds, 2)
                       , (Pomeranians, 3)
                       , (Akitas, 0)
                       , (Vizslas, 0)
                       , (Goldfish, 5)
                       , (Trees, 3)
                       , (Cars, 2)
                       , (Perfumes, 1)]

isSubset :: Poss -> Poss -> Bool
isSubset small big = foldrWithKey (f big) True small
  where
    f :: Poss -> Obj -> Int -> Bool -> Bool
    f p obj quantity = (&&) (quantity == lookupDefault (-1) obj p)
    --- ^ implicit assumption that quantity is never negative
    --- so if the lookup fails then the whole predicate fails

correctedMatch :: Poss -> Poss -> Bool
correctedMatch record detection = foldrWithKey (f detection) True record
  where
    l :: Obj -> Poss -> Int
    l = lookupDefault (-1)
    f :: Poss -> Obj -> Int -> Bool -> Bool
    f p obj q | obj == Cats || obj == Trees = (&&) (q >= l obj p)
              | obj == Pomeranians || obj == Goldfish = (&&) (q <= l obj p)
              | otherwise = (&&) (q == l obj p)


main :: IO ()
main = do
  d <- readData "inputs/day16"
  printWithPrefix " part1: " $ HM.filter (`isSubset` scannerData) d
  printWithPrefix " part2: " $ HM.filter (`correctedMatch` scannerData) d
