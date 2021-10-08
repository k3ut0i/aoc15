module Day19 ( Chemistry
             , allReplacements) where
import Utils (printWithPrefix, readLines, splitOn, Identity)
import Text.ParserCombinators.ReadP ( ReadP, string, choice, pfail
                                    , readP_to_S, manyTill, eof, (<++)
                                    , satisfy)
import Data.List (nub)
  
readTrans :: String -> (String, String)
readTrans s = let [e, "=>", m] = splitOn ' ' s in (e, m)

type Chemistry = [(String, String)]

readData :: FilePath -> IO (Chemistry, String)
readData f = splitAndRead . splitOn "" <$> readLines f
  where
    splitAndRead [es, [s]] = (map readTrans es, s)
    splitAndRead _ = undefined --- shouldn't fire for the correct input

lookUpStrings :: [String] -> ReadP String
lookUpStrings = choice . map string

parseAll :: [String] -> String -> [String]
parseAll d s = case readP_to_S (manyTill (lookUpStrings d <++ cs) eof) s of
                 [(ans, [])] -> ans
                 a -> error $ show a
  where
    cs :: ReadP String
    cs = (:[]) <$> satisfy (const True)

parseData :: Chemistry -> String -> [String]
parseData c = parseAll (nub $ map fst c)


-- replace one instance of a1 with a2 in the list
replaceOnce :: (Eq a) => a -> a -> [a] -> [[a]]
replaceOnce a1 a2 [] = [[]]
replaceOnce a1 a2 (x:xs)
  | x == a1 = (a2:xs) : map (x:) (replaceOnce a1 a2 xs)
  | otherwise = map (x:) $ replaceOnce a1 a2 xs

{- This replaces all the matching molecules -}
-- replace :: Chemistry -> String -> String
-- replace c s = concatMap (\x -> lookupDefault x x c) $ parseData c s

-- replace the molecule m in the string s
-- replaceOne :: [String] -> Chemistry -> Identity (HashMap String [[String]])
-- replaceOne s = traverseWithKey (\k v -> pure $ replaceOnce k v s)

-- parsed-string and chemistry data
replace :: [String] -> Chemistry -> [String]
replace ss = concatMap (\(mol, rep) ->
                          map concat $ replaceOnce mol rep ss)


allReplacements :: Chemistry -> String -> [String]
allReplacements c s = replace (parseData c s) c

main :: IO ()
main = readData "inputs/day19" >>= printWithPrefix " part1: " . f
  where
    f = flip (-) 1 . length . nub . uncurry allReplacements
    -- ^ -1 due to the presence of the original string 
