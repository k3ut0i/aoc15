module Day14 () where

import Text.ParserCombinators.ReadP (ReadP, string, readP_to_S,
                                     skipSpaces)
import Text.Read.Lex (hsLex, readDecP)
import Utils (readLines, printWithPrefix)
import Data.HashMap.Strict (HashMap(..), fromList, (!), keys)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

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
    then (full * km * sec) + (part * km) -- ignoring the fractional parts
    else (full + 1) * km * sec
    

distAfter :: String -> Int -> Reader Data Int
distAfter n t =  getDis t <$> asks (!n)

part1 :: Int -> Reader Data [(String, Int)]
part1 t = do
  d <- ask
  mapM (\e -> (,) e <$> distAfter e t) (keys d)

main :: IO ()
main = do d <- readData "inputs/day14"
          printWithPrefix " part1: " $ (maximum . map snd) (runReader (part1 2503) d)
          
type SystemState = HashMap String (Int, Int, Int) -- (time, dist, score)

stateStep :: StateT SystemState (Reader Data) ()
stateStep = undefined
