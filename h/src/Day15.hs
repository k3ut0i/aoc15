module Day15 () where
import Text.ParserCombinators.ReadP (ReadP, string, readS_to_P,
                                     readP_to_S, skipSpaces)
import Text.Read.Lex (hsLex, readDecP)
import Utils (readLines, printWithPrefix)
import Data.HashMap.Strict (HashMap, fromList, foldrWithKey, (!), keys)
import Control.Monad.Trans.Reader
import Control.Applicative (liftA2)
data P = P { capacity :: Int
           , durability :: Int
           , flavor :: Int
           , texture :: Int
           , calories :: Int
           } deriving (Eq, Show)

type Data = HashMap String P

readIP :: ReadP Int
readIP = readS_to_P reads

readEntryP :: ReadP (String, P)
readEntryP = do
  name <- hsLex
  skipSpaces >> string ": capacity"
  capacity <- readIP
  string ", durability"
  durability <- readIP
  string ", flavor"
  flavor <- readIP
  string ", texture"
  texture <- readIP
  string ", calories"
  calories <- readIP
  return (name, P capacity  durability flavor texture calories)

readEntry :: String -> (String, P)
readEntry s = case readP_to_S readEntryP s of
                [(e, [])] -> e
                _ -> error "could not parse entry"

readData :: FilePath -> IO Data
readData f = fromList . map readEntry <$> readLines f

type Recipie = HashMap String Int

weigh :: Int -> P -> P
weigh w (P cp du fl te ca) = P (w*cp) (w*du) (w*fl) (w*te) (w*ca)

addP :: P -> P -> P
addP (P cp1 du1 fl1 te1 ca1) (P cp2 du2 fl2 te2 ca2) =
  P (cp1+cp2) (du1+du2) (fl1+fl2) (te1+te2) (ca1+ca2)

addI :: Int -> P -> P -> P
addI w ing = addP (weigh w ing)

score :: P -> Int
score (P cp du fl te _) | any (<0) [cp, du, fl, te] = 0
                        | otherwise = cp * du * fl * te

scoreRecipie :: Recipie ->  Reader Data Int
scoreRecipie r =
  score . foldrWithKey (\k -> addI (r!k)) (P 0 0 0 0 0) <$> ask

calorieCount :: Recipie -> Reader Data Int
calorieCount r = do
  d <- ask
  return $ foldrWithKey (\k v a -> a + v * calories (d!k)) 0 r
  
distribute ::Int -> [[Int]]
distribute n = [[a1, a2, a3, a4] | a1 <- [0 .. n] 
                                 , a2 <- [0 .. n]
                                 , a3 <- [0 .. n]
                                 , a4 <- [0 .. n]
                                 , a1 + a2 + a3 + a4 == n]
distribute2 ::Int -> [[Int]]
distribute2 n = [[a1, a2] | a1 <- [1 .. n] 
                          , a2 <- [1 .. n]
                          , a1 + a2 == n]

bestRecipie :: Reader Data (Int, Int)
bestRecipie = do
  d <- ask
  let possible = map (zip (keys d)) (distribute 100)
  let part1 = maximum <$> mapM f possible
  let part2 = maximum <$> (mapM f . filter (g d)) possible
  liftA2 (,) part1 part2
  where
    f :: [(String, Int)] -> Reader Data Int
    f l = runReader (scoreRecipie (fromList l)) <$> ask
    g :: Data -> [(String, Int)] -> Bool
    g d l = 500 == runReader (calorieCount (fromList l)) d

main :: IO ()
main = readData "inputs/day15" >>=
       printWithPrefix " part1: " . runReader bestRecipie
