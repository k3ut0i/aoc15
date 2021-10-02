module Day13 () where

import Text.ParserCombinators.ReadP (ReadP, string, readP_to_S,
                                     skipSpaces)
import Text.Read.Lex (hsLex, readDecP)
import Utils (readLines, printWithPrefix)
import Data.List (permutations, nub)

type Entry = (String, String, Int)

readEntryP :: ReadP Entry
readEntryP = do
  subject <- hsLex
  skipSpaces >> string "would" >> skipSpaces
  gainOrLose <- hsLex
  skipSpaces
  amount <- readDecP
  skipSpaces >> string "happiness units by sitting next to" >> skipSpaces
  object <- hsLex
  string "."
  return (subject, object, if gainOrLose == "gain"
                           then amount
                           else -1*amount)

readEntry :: String -> Maybe Entry
readEntry s = case readP_to_S readEntryP s of
                [(a, [])] -> Just a
                _ -> Nothing

readData :: FilePath -> IO (Maybe [Entry])
readData f = mapM readEntry <$> readLines f

collectNames :: [Entry] -> [String]
collectNames = nub . map (\(e, _, _) -> e)

getS [] sub obj = error $ "Cannot find data:" ++ sub ++ "->" ++ obj
getS ((sub, obj, hap):r) sub1 obj1 | sub == sub1 && obj == obj1 = hap
                                   | otherwise = getS r sub1 obj1

score' :: [Entry] -> String -> [String] -> Int
score' es first [penul, last] = getS es last penul + getS es last first + getS es first last
score' es first (prev:current:next:rest) = getS es current prev +
                                           getS es current next +
                                           score' es first (current:next:rest)
score' es _ _ = error "recursion should not have gotten here "

score :: [Entry] -> [String] -> Int
score es (x:y:xs) = score' es x (x:y:xs) + getS es x y
score es _ = error "recursion should not have gotten here "

optScore :: [Entry] -> [[String]] -> Int
optScore es = maximum . map (score es)

part1 :: [Entry] -> Int
part1 es = optScore es (permutations (collectNames es))

main :: IO ()
main = do
  es <- readData "inputs/day13"
  printWithPrefix " part1: " (fmap part1 es)
  printWithPrefix " part2: " (fmap part1 (addNeutral <$> es))
  where
    addNeutral :: [Entry] -> [Entry]
    addNeutral es = es ++ concatMap (\name -> [("Raja", name, 0), (name, "Raja", 0)]) (collectNames es)

-- TODO: rewrite score using the following to make it more clear
-- [1, 2, 3, 4] -> [(1, 4), (1, 2), (2, 1), (2, 3), (3, 2), (3, 4), (4, 3), (4, 1)]
circularPairings' :: a -> [a] -> [(a, a)]
circularPairings' w (x:y:z:r) = (y,x):(y,z):circularPairings' w (y:z:r)
circularPairings' w [x, y] = [(y,x), (y, w), (w, y)]

circularPairings :: [a] -> [(a,a)]
circularPairings (x:y:xs) = (x, y) : circularPairings' x (x:y:xs)
