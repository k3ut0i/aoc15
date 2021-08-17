{-# LANGUAGE LambdaCase #-}
module Day7 (
  main,
  Con(..),
  ConIn(..),
  evalS,
  eval
    ) where

import Text.ParserCombinators.ReadP (readP_to_S,
                                     ReadP,
                                     string,
                                     skipSpaces,
                                    (+++), (<++))
import Text.Read.Lex (hsLex, readDecP)
import Data.Maybe (mapMaybe, fromJust)
import Data.Bits ((.&.), (.|.), shiftL, shiftR, complement, Bits (shiftL))
import Data.Word (Word16)
import qualified Data.Map as M (Map(..), lookup, insert, empty)
import Utils (readLines, printWithPrefix)

type WireVal = Word16
data Wire = Name String | Val WireVal --- XXX: Some gates have direct values attached
          deriving(Show, Eq)


instance Read Wire where
  readsPrec _ = readP_to_S readPWire

readPWire :: ReadP Wire
readPWire = (Val <$> readDecP) <++ (Name <$> hsLex) -- first time using <++

data BinOp = AND | OR | LSHIFT | RSHIFT
           deriving (Show, Eq)

data ConIn = Bin BinOp Wire Wire
           | NOT Wire
           | RAW Wire
           deriving (Show, Eq)

readConIn :: ReadP ConIn
readConIn = rBin +++ rNot +++ rRaw
  where
    rRaw = RAW <$> readPWire
    rNot = string "NOT" >> skipSpaces >> NOT <$> readPWire
    rBin = do
      s1 <- readPWire
      skipSpaces
      b <- (string "AND" >> return AND) +++
           (string "OR" >> return OR) +++
           (string "LSHIFT" >> return LSHIFT) +++
           (string "RSHIFT" >> return RSHIFT)
      skipSpaces
      Bin b s1 <$> readPWire

instance Read ConIn where
  readsPrec _ = readP_to_S readConIn

collectwires :: [Wire] -> [String]
collectwires = mapMaybe (\case { Name s -> Just s; Val _ -> Nothing})

inputs :: ConIn -> [String]
inputs (RAW _) = []
inputs (NOT w) = collectwires [w]
inputs (Bin _ w1 w2) = collectwires [w1, w2]

depCon :: Con -> [String]
depCon (Con s ci) = inputs ci

data Con = Con String ConIn
         deriving (Eq)
readPCon :: ReadP Con
readPCon = do
  c <- readConIn
  skipSpaces >> string "->" >> skipSpaces
  s <- hsLex
  return $ Con s c

instance Show Con where
  show (Con s i) = show i ++ " -> " ++ s
instance Read Con where
  readsPrec _ = readP_to_S readPCon

isCon :: String -> Con -> Bool
isCon s (Con s1 _) = s == s1

getCon :: String -> Network -> Maybe Con
getCon s n = case filter (isCon s) n of
               [c] -> Just c
               _ -> Nothing

type Network = [Con]
type Env = M.Map String WireVal

--- TODO: See if using state and reader monads makes the complexity less
--- XXX: Let me try this after getting a solution with just passing parameters
-- eval :: String -> StateT Env (Reader Network) WireVal
-- eval s = do
--   (Con _ c) <- return (getCon s)
--   return 1

evalBin :: BinOp -> WireVal -> WireVal -> WireVal
evalBin AND = (.&.)
evalBin OR = (.|.)
evalBin LSHIFT = \a b -> shiftL a (fromInteger $ toInteger b)
evalBin RSHIFT = \a b -> shiftR a (fromInteger $ toInteger b)

evalS :: String -> Network -> Maybe WireVal
evalS s n = eval s n M.empty >>= \(i, _) -> return i

eval :: String -> Network -> Env -> Maybe (WireVal, Env)
eval s n e = case M.lookup s e of
  Nothing ->  getCon s n >>= \c -> evalCon c n e
  Just i -> Just (i, e)

evalWire :: Wire -> Network -> Env -> Maybe (WireVal, Env)
evalWire (Name s) n e = eval s n e
evalWire (Val i) _ e = Just (i, e)

evalCon :: Con -> Network -> Env -> Maybe (WireVal, Env)
evalCon (Con s (RAW w)) n e =  evalWire w n e >>=
                               \(i, en) -> return (i, M.insert s i en)
evalCon (Con s (NOT w)) n e =
  evalWire w n e >>=
  \(i, en) -> return (not16 i, M.insert s i en)
  where
    not16 :: WireVal -> WireVal
    not16 x = complement x

evalCon (Con s (Bin b w1 w2)) n e = do
  (i1, e1) <- evalWire w1 n e
  (i2, e2) <- evalWire w2 n e1
  let v = evalBin b i1 i2 in
    return (v, M.insert s v e2)


part1 :: FilePath -> IO String
part1 f = readLines f >>= (return . show . evalS "a") . map (\s -> read s :: Con)


main :: IO ()
main = do
  part1ans <- evalS "a" <$> network
  part2ans <- evalS "a" . map (replaceb $ fromJust part1ans) <$> network
  putStr " part1: " >> print part1ans >> putStr " part2: " >> print part2ans
  where
    network = map read <$> readLines "inputs/day7"
    replaceb v c@(Con s i) = if s == "b" then Con "b" (RAW (Val v)) else c

