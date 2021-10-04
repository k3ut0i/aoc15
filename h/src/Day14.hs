module Day14 () where

import Text.ParserCombinators.ReadP (ReadP, string, readP_to_S,
                                     skipSpaces)
import Text.Read.Lex (hsLex, readDecP)
import Utils (readLines, printWithPrefix)
import Data.HashMap.Strict as HM (HashMap(..), fromList, (!),
                                  keys, foldl', unionWith)
-- import qualified Data.HashMap.Strict as HM (map, mapWithKey)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Identity

type SpeedParam = (Int, Int, Int) -- (km, in-n-secs, rest-secs)
type Entry = (String, SpeedParam)

readEntryP :: ReadP Entry
readEntryP = do
  name <- hsLex
  skipSpaces >> string "can fly" >> skipSpaces
  km <- readDecP
  skipSpaces >> string "km/s for" >> skipSpaces
  inSecs <- readDecP
  skipSpaces  >> string "seconds, but then must rest for" >> skipSpaces
  restSecs <- readDecP
  skipSpaces  >> string "seconds."
  return (name, (km, inSecs, restSecs))
  
readEntry :: String -> Entry
readEntry s = case readP_to_S readEntryP s of
                [(e, [])] -> e
                _ -> error "could not parse entry"

type Data = HashMap String SpeedParam

readData :: FilePath -> IO Data
readData f = fromList . map readEntry <$> readLines f

getDis :: Int -> (Int, Int, Int) -> Int
getDis time (km, sec, rest) =
  let (full, part) = time `quotRem` (sec + rest) in
    if part <= sec
    then (full * km * sec) + (part * km)
    else (full + 1) * km * sec
    

distAfter :: String -> Int -> Reader Data Int
distAfter n t =  getDis t <$> asks (!n)

part1 :: Int -> Reader Data [(String, Int)]
part1 t = do
  d <- ask
  mapM (\e -> (,) e <$> distAfter e t) (keys d)
          
type Score = HashMap String Int

scoreAdjust :: Int -> Data -> Score -> Score
scoreAdjust time d = unionWith (+) scoreDiff
  where
    scoreDiff = fmap (\v -> if v == maxDis then 1 else 0) dis
    dis = fmap (getDis time) d
    maxDis = foldl' max 0 dis

scoreAt :: Int -> Data -> Score
scoreAt 0 d = fmap (const 0) d
scoreAt time d = scoreAdjust time d (scoreAt (time - 1) d)

main :: IO ()
main = do d <- readData "inputs/day14"
          printWithPrefix " part1: " $ (maximum . map snd)
            (runReader (part1 2503) d)
          printWithPrefix " part2: " $ scoreAt 2503 d
