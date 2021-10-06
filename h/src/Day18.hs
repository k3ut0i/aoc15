module Day18 () where

import Utils (printWithPrefix, readLines, nTimes)
import Data.Array
import Control.Monad.Trans.State
import Control.Applicative (liftA2)
type Lights = Array (Int, Int) Bool

readMap :: FilePath -> IO Lights
readMap file = do
  ls <- readLines file
  let size = length ls
  return $ array ((0, 0), (size-1, size-1)) (f ls)
    where
      f :: [String] -> [((Int, Int), Bool)]
      f =  map (fmap (== '#')) . g . zip [0..]
      g :: [(Int, String)] -> [((Int, Int), Char)]
      g = concatMap (\(i, s) -> zipWith (\j c -> ((i, j), c)) [0..] s)

neighbours :: (Int, Int) -> Lights -> [(Int, Int)]
neighbours (i, j) l = 
  filter (inRange $ bounds l)
  [(i+id, j+jd) | id <- [-1, 0, 1], jd <- [-1, 0, 1], (id,jd) /= (0,0)]

rule :: Bool -> Int -> Bool
rule True n | n == 2 || n == 3 = True
            | otherwise = False
rule False 3 = True
rule False _ = False

stepPos :: (Int, Int) -> Lights -> Bool
stepPos idx lights =   let n = neighbours idx lights in
  rule (lights!idx) (length . filter id $ map (lights!) n)

step :: Lights -> Lights
step l = let is = indices l in
           l // zip is (map (`stepPos` l) is)
  
stepn :: Int -> Lights -> Lights
stepn n = nTimes n step

countLights :: Lights -> Int
countLights = foldl (\a b -> if b then a+1 else a) 0

part1 :: Lights -> Int
part1 =  countLights . stepn 100

stuck :: Lights -> Lights
stuck l = let ((0, 0), (b1, b2)) = bounds l in
  l//[ ((0, 0), True), ((0, b2), True)
     , ((b1, 0), True), ((b1, b2), True)]

stepStuckN :: Int -> Lights -> Lights
stepStuckN n l = nTimes n (stuck . step) (stuck l)

part2 :: Lights -> Int
part2 = countLights . stepStuckN 100

main :: IO ()
main = readMap "inputs/day18" >>=
  \l -> printWithPrefix " part1: " (part1 l) >>
        printWithPrefix " part2: " (part2 l)

printMap :: Lights -> IO ()
printMap ls = let ((0, 0), (b1, b2)) = bounds ls in
                mapM_ (\a ->  mapM_ pc a >> putStrLn "")
                [ [ls!(i,j) | j <- [0..b2]] -- there should be a better way to iterate column by column
                | i <- [0..b1]]
  where
    pc :: Bool -> IO ()
    pc b = putStr $ if b then "#" else "."
